package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.BuildInfo
import ch.epfl.scala.debugadapter.ClassEntry
import ch.epfl.scala.debugadapter.DebugConfig
import ch.epfl.scala.debugadapter.DebugTools
import ch.epfl.scala.debugadapter.Debuggee
import ch.epfl.scala.debugadapter.EvaluationFailed
import ch.epfl.scala.debugadapter.JavaRuntime
import ch.epfl.scala.debugadapter.Logger
import ch.epfl.scala.debugadapter.ManagedEntry
import ch.epfl.scala.debugadapter.UnmanagedEntry
import ch.epfl.scala.debugadapter.internal.evaluator.CompiledExpression
import ch.epfl.scala.debugadapter.internal.evaluator.FrameReference
import ch.epfl.scala.debugadapter.internal.evaluator.JdiObject
import ch.epfl.scala.debugadapter.internal.evaluator.LocalValue
import ch.epfl.scala.debugadapter.internal.evaluator.MessageLogger
import ch.epfl.scala.debugadapter.internal.evaluator.MethodInvocationFailed
import ch.epfl.scala.debugadapter.internal.evaluator.PlainLogMessage
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

import ScalaExtension.*
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.util.Try

private[internal] class EvaluationProvider(
    sourceLookUp: SourceLookUpProvider,
    simpleEvaluator: SimpleEvaluator,
    messageLogger: MessageLogger,
    scalaEvaluators: Map[ClassEntry, ScalaEvaluator],
    mode: DebugConfig.EvaluationMode,
    logger: Logger
)(implicit context: ExecutionContext)
    extends IEvaluationProvider {

  private var debugContext: IDebugAdapterContext = _
  private val isEvaluating = new AtomicBoolean(false)

  private def fromTryToFuture[T](t: => Try[T]) = {
    t match {
      case Success(value) => Future.successful(value)
      case Failure(e) => Future.failed(e)
    }
  }

  override def initialize(debugContext: IDebugAdapterContext, options: java.util.Map[String, AnyRef]): Unit =
    this.debugContext = debugContext

  override def isInEvaluation(thread: ThreadReference) = isEvaluating.get

  override def evaluate(expression: String, thread: ThreadReference, depth: Int): CompletableFuture[Value] = {
    val frame = FrameReference(thread, depth)
    val evaluation = fromTryToFuture {
      for {
        preparedExpression <- prepare(expression, frame)
        evaluation <- evaluate(preparedExpression, frame)
      } yield evaluation
    }
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
        breakpoint.getCompiledExpression(locationCode).asInstanceOf[Try[PreparedExpression]]
      } else if (breakpoint.containsConditionalExpression) {
        prepare(breakpoint.getCondition, frame)
      } else if (breakpoint.containsLogpointExpression) {
        prepareLogMessage(breakpoint.getLogMessage, frame)
      } else {
        Failure(new Exception("Missing expression"))
      }
    breakpoint.setCompiledExpression(locationCode, expression)
    val evaluation = fromTryToFuture {
      for {
        expression <- expression
        evaluation <- evaluate(expression, frame)
      } yield evaluation
    }
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
    completeFuture(fromTryToFuture(invocation.getResult), thread)
  }

  private def getScalaEvaluator(fqcn: String): Try[ScalaEvaluator] =
    for {
      entry <- sourceLookUp.getClassEntry(fqcn).toTry(s"Unknown class $fqcn")
      evaluator <- scalaEvaluators.get(entry).toTry(missingEvaluatorMessage(entry))
    } yield evaluator

  private def missingEvaluatorMessage(entry: ClassEntry): String =
    entry match {
      case m: ManagedEntry =>
        m.scalaVersion match {
          case None => s"Unsupported evaluation in Java classpath entry: ${entry.name}"
          case Some(sv) =>
            s"""|Missing scala-expression-compiler_{$sv} with version ${BuildInfo.version}.
                |You can open an issue at https://github.com/scalacenter/scala-debug-adapter.""".stripMargin
        }
      case _: JavaRuntime => s"Unsupported evaluation in JDK: ${entry.name}"
      case _: UnmanagedEntry => s"Unsupported evaluation in unmanaged classpath entry: ${entry.name}"
      case _ => s"Unsupported evaluation in ${entry.name}"
    }

  private def prepareLogMessage(message: String, frame: FrameReference): Try[PreparedExpression] = {
    if (!message.contains("$")) {
      Success(PlainLogMessage(message))
    } else {
      val tripleQuote = "\"\"\""
      val expression = s"""println(s$tripleQuote$message$tripleQuote)"""
      prepare(expression, frame)
    }
  }

  private def prepare(expression: String, frame: FrameReference): Try[PreparedExpression] = {
    lazy val simpleExpression = simpleEvaluator.prepare(expression, frame)
    if (mode.allowSimpleEvaluation && simpleExpression.isDefined) {
      Success(simpleExpression.get)
    } else if (mode.allowScalaEvaluation) {
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
      case logMessage: PlainLogMessage => messageLogger.log(logMessage, frame)
      case localValue: LocalValue => simpleEvaluator.evaluate(localValue, frame)
      case expression: CompiledExpression =>
        val fqcn = frame.current().location.declaringType.name
        for {
          evaluator <- getScalaEvaluator(fqcn)
          compiledExpression <- evaluationBlock { evaluator.evaluate(expression, frame) }
        } yield compiledExpression
    }
  }

  private def completeFuture[T](result: Future[T], thread: ThreadReference): CompletableFuture[T] = {
    val future = new CompletableFuture[T]()
    debugContext.getStackFrameManager.reloadStackFrames(thread)
    result.onComplete {
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
  )(implicit context: ExecutionContext): IEvaluationProvider = {
    val simpleEvaluator = new SimpleEvaluator(logger, config.testMode)
    val scalaEvaluators = debugTools.expressionCompilers.view.map { case (entry, compiler) =>
      (entry, new ScalaEvaluator(entry, compiler, logger, config.testMode))
    }.toMap
    val messageLogger = new MessageLogger()
    new EvaluationProvider(
      sourceLookUp,
      simpleEvaluator,
      messageLogger,
      scalaEvaluators,
      config.evaluationMode,
      logger
    )
  }
}
