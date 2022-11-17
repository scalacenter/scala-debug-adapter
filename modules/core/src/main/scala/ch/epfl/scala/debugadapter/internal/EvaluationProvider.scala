package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.ClassEntry
import ch.epfl.scala.debugadapter.DebugConfig
import ch.epfl.scala.debugadapter.DebugTools
import ch.epfl.scala.debugadapter.Debuggee
import ch.epfl.scala.debugadapter.EvaluationFailed
import ch.epfl.scala.debugadapter.Logger
import ch.epfl.scala.debugadapter.internal.evaluator.CompiledExpression
import ch.epfl.scala.debugadapter.internal.evaluator.FrameReference
import ch.epfl.scala.debugadapter.internal.evaluator.JdiObject
import ch.epfl.scala.debugadapter.internal.evaluator.LocalValue
import ch.epfl.scala.debugadapter.internal.evaluator.MethodInvocationFailed
import ch.epfl.scala.debugadapter.internal.evaluator.PreparedExpression
import ch.epfl.scala.debugadapter.internal.evaluator.ScalaEvaluator
import ch.epfl.scala.debugadapter.internal.evaluator.SimpleEvaluator
import com.microsoft.java.debug.core.IEvaluatableBreakpoint
import com.microsoft.java.debug.core.adapter.IDebugAdapterContext
import com.microsoft.java.debug.core.adapter.IEvaluationProvider
import com.sun.jdi.ObjectReference
import com.sun.jdi.ThreadReference
import com.sun.jdi.Value

import java.util.concurrent.CompletableFuture
import java.util.concurrent.atomic.AtomicBoolean
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import ScalaExtension.*

private[internal] class EvaluationProvider(
    sourceLookUp: SourceLookUpProvider,
    simpleEvaluator: SimpleEvaluator,
    scalaEvaluators: Map[ClassEntry, ScalaEvaluator],
    mode: DebugConfig.EvaluationMode,
    logger: Logger
) extends IEvaluationProvider {

  private var debugContext: IDebugAdapterContext = _
  private val isEvaluating = new AtomicBoolean(false)

  override def initialize(debugContext: IDebugAdapterContext, options: java.util.Map[String, AnyRef]): Unit =
    this.debugContext = debugContext

  override def isInEvaluation(thread: ThreadReference) = isEvaluating.get

  override def evaluate(expression: String, thread: ThreadReference, depth: Int): CompletableFuture[Value] = {
    val frame = FrameReference(thread, depth)
    val evaluation = for {
      preparedExpression <- prepare(expression, frame)
      evaluation <- evaluate(preparedExpression, frame)
    } yield evaluation
    completeFuture(evaluation, thread)
  }

  override def evaluate(
      expression: String,
      thisContext: ObjectReference,
      thread: ThreadReference
  ): CompletableFuture[Value] = ???

  override def evaluateForBreakpoint(
      breakpoint: IEvaluatableBreakpoint,
      thread: ThreadReference
  ): CompletableFuture[Value] = {
    val frame = FrameReference(thread, 0)
    val location = frame.current().location
    val locationCode = (location.method.name, location.codeIndex).hashCode
    val expression =
      if (breakpoint.getCompiledExpression(locationCode) != null) {
        breakpoint.getCompiledExpression(locationCode).asInstanceOf[Try[CompiledExpression]]
      } else if (breakpoint.containsConditionalExpression) {
        prepare(breakpoint.getCondition, frame)
      } else if (breakpoint.containsLogpointExpression) {
        val tripleQuote = "\"\"\""
        val expression = s"""println(s$tripleQuote${breakpoint.getLogMessage}$tripleQuote)"""
        prepare(expression, frame)
      } else {
        Failure(new Exception("Missing expression"))
      }
    breakpoint.setCompiledExpression(locationCode, expression)
    val evaluation = for {
      expression <- expression
      evaluation <- evaluate(expression, frame)
    } yield evaluation
    completeFuture(evaluation, thread)
  }

  override def invokeMethod(
      thisContext: ObjectReference,
      methodName: String,
      methodSignature: String,
      args: Array[Value],
      thread: ThreadReference,
      invokeSuper: Boolean
  ): CompletableFuture[Value] = {
    val obj = new JdiObject(thisContext, thread)
    val invocation = evaluationBlock {
      obj
        .invoke(methodName, methodSignature, if (args == null) List() else args.toList)
        .recover {
          // if invocation throws an exception, we return that exception as the result
          case MethodInvocationFailed(msg, exception) => exception
        }
    }
    completeFuture(invocation.getResult, thread)
  }

  private def getScalaEvaluator(fqcn: String): Try[ScalaEvaluator] =
    for {
      entry <- sourceLookUp.getClassEntry(fqcn).toTry(s"Unknown class $fqcn")
      evaluator <- scalaEvaluators.get(entry).toTry(s"Missing expression compiler for entry ${entry.name}")
    } yield evaluator

  private def prepare(expression: String, frame: FrameReference): Try[PreparedExpression] = {
    lazy val simpleExpression = simpleEvaluator.prepare(expression, frame)
    if (mode.canBypassCompiler && simpleExpression.isDefined) {
      Success(simpleExpression.get)
    } else if (mode.canUseCompiler) {
      val fqcn = frame.current().location.declaringType.name
      for {
        evaluator <- getScalaEvaluator(fqcn)
        sourceContent <- sourceLookUp
          .getSourceContentFromClassName(fqcn)
          .toTry(s"Cannot find source file of class $fqcn")
        preparedExpression <- evaluator.compile(sourceContent, expression, frame)
      } yield preparedExpression
    } else {
      Failure(new EvaluationFailed(s"Cannot evaluate '$expression' with $mode mode"))
    }
  }

  private def evaluate(expression: PreparedExpression, frame: FrameReference): Try[Value] = {
    expression match {
      case localValue: LocalValue => simpleEvaluator.evaluate(localValue, frame)
      case expression: CompiledExpression =>
        val fqcn = frame.current().location.declaringType.name
        for {
          evaluator <- getScalaEvaluator(fqcn)
          compiledExpression <- evaluationBlock { evaluator.evaluate(expression, frame) }
        } yield compiledExpression
    }
  }

  private def completeFuture[T](result: Try[T], thread: ThreadReference): CompletableFuture[T] = {
    val future = new CompletableFuture[T]()
    debugContext.getStackFrameManager.reloadStackFrames(thread)
    result match {
      case Success(value) => future.complete(value)
      case Failure(exception) => future.completeExceptionally(exception)
    }
    future
  }

  private def evaluationBlock[T](f: => T): T = {
    isEvaluating.set(true)
    try f
    finally { isEvaluating.set(false) }
  }

  override def clearState(thread: ThreadReference): Unit = {}
}

private[internal] object EvaluationProvider {
  def apply(
      debuggee: Debuggee,
      debugTools: DebugTools,
      sourceLookUp: SourceLookUpProvider,
      logger: Logger,
      config: DebugConfig
  ): IEvaluationProvider = {
    val simpleEvaluator = new SimpleEvaluator(logger, config.testMode)
    val scalaEvaluators = debugTools.expressionCompilers.view.map { case (entry, compiler) =>
      (entry, new ScalaEvaluator(entry, compiler, logger, config.testMode))
    }.toMap
    new EvaluationProvider(sourceLookUp, simpleEvaluator, scalaEvaluators, config.evaluationMode, logger)
  }
}
