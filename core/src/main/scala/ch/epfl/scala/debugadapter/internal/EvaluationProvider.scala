package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.Debuggee
import ch.epfl.scala.debugadapter.internal.evaluator.ExpressionEvaluator
import ch.epfl.scala.debugadapter.internal.evaluator.JdiObject
import ch.epfl.scala.debugadapter.internal.evaluator.MethodInvocationFailed
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
import ch.epfl.scala.debugadapter.Logger
import scala.util.Try
import ScalaExtension._
import ch.epfl.scala.debugadapter.DebugTools
import ch.epfl.scala.debugadapter.ClassEntry
import ch.epfl.scala.debugadapter.internal.evaluator.CompiledExpression

private[internal] class EvaluationProvider(
    sourceLookUp: SourceLookUpProvider,
    evaluators: Map[ClassEntry, ExpressionEvaluator],
    logger: Logger
) extends IEvaluationProvider {

  private var debugContext: IDebugAdapterContext = _
  private val isEvaluating = new AtomicBoolean(false)

  override def initialize(debugContext: IDebugAdapterContext, options: java.util.Map[String, AnyRef]): Unit =
    this.debugContext = debugContext

  override def isInEvaluation(thread: ThreadReference) = isEvaluating.get

  override def evaluate(expression: String, thread: ThreadReference, depth: Int): CompletableFuture[Value] = {
    val evaluation = for {
      compiledExpression <- compileExpression(expression, thread, depth)
      evaluation <- evaluate(compiledExpression, thread, depth)
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
    val depth = 0
    val location = thread.frame(depth).location
    val locationCode = (location.method.name, location.codeIndex).hashCode
    val compiledExpression: Try[CompiledExpression] =
      if (breakpoint.getCompiledExpression(locationCode) != null) {
        breakpoint.getCompiledExpression(locationCode).asInstanceOf[Try[CompiledExpression]]
      } else if (breakpoint.containsConditionalExpression) {
        compileExpression(breakpoint.getCondition, thread, depth)
      } else if (breakpoint.containsLogpointExpression) {
        val tripleQuote = "\"\"\""
        val expression = s"""println(s$tripleQuote${breakpoint.getLogMessage}$tripleQuote)"""
        compileExpression(expression, thread, depth)
      } else {
        Failure(new Exception("Missing expression"))
      }
    breakpoint.setCompiledExpression(locationCode, compiledExpression)
    val evaluation = for {
      compiledExpression <- compiledExpression
      evaluation <- evaluate(compiledExpression, thread, depth)
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

  private def compileExpression(expression: String, thread: ThreadReference, depth: Int): Try[CompiledExpression] = {
    val location = thread.frame(depth).location
    val fqcn = location.declaringType.name

    for {
      entry <- sourceLookUp.getClassEntry(fqcn).toTry(s"Unknown class $fqcn")
      evaluator <- evaluators.get(entry).toTry(s"Missing evaluator for entry ${entry.name}")
      sourceContent <- sourceLookUp.getSourceContentFromClassName(fqcn).toTry(s"Cannot find source file of class $fqcn")
      compiledExpression <- evaluationBlock { evaluator.compile(sourceContent, expression, thread, depth) }
    } yield compiledExpression
  }

  private def evaluate(compiledExpression: CompiledExpression, thread: ThreadReference, depth: Int): Try[Value] = {
    val location = thread.frame(depth).location
    val fqcn = location.declaringType.name

    for {
      entry <- sourceLookUp.getClassEntry(fqcn).toTry(s"Unknown class $fqcn")
      evaluator <- evaluators.get(entry).toTry(s"Missing evaluator for entry ${entry.name}")
      compiledExpression <- evaluationBlock { evaluator.evaluate(compiledExpression, thread, depth) }
    } yield compiledExpression
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
      testMode: Boolean
  ): IEvaluationProvider = {
    val allEvaluators = debugTools.expressionCompilers.view.map { case (k, v) =>
      (k, new ExpressionEvaluator(v, logger, testMode))
    }.toMap
    new EvaluationProvider(sourceLookUp, allEvaluators, logger)
  }
}
