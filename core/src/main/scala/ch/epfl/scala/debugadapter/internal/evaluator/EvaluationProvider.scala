package ch.epfl.scala.debugadapter.internal.evaluator

import ch.epfl.scala.debugadapter.DebuggeeRunner
import ch.epfl.scala.debugadapter.internal.SourceLookUpProvider
import com.microsoft.java.debug.core.IEvaluatableBreakpoint
import com.microsoft.java.debug.core.adapter.{
  IDebugAdapterContext,
  IEvaluationProvider
}
import com.sun.jdi.{ObjectReference, ThreadReference, Value}

import java.util
import java.util.concurrent.CompletableFuture

object EvaluationProvider {
  def apply(
      runner: DebuggeeRunner,
      sourceLookUpProvider: SourceLookUpProvider
  ): IEvaluationProvider = {
    val evaluator = for {
      classLoader <- runner.evaluationClassLoader
      expressionCompiler <- ExpressionCompiler(classLoader)
    } yield new Evaluator(sourceLookUpProvider, expressionCompiler)
    evaluator.map(new EvaluationProvider(_)).getOrElse(NoopEvaluationProvider)
  }
}

class EvaluationProvider(evaluator: Evaluator) extends IEvaluationProvider {

  private var debugContext: IDebugAdapterContext = _

  override def initialize(
      debugContext: IDebugAdapterContext,
      options: util.Map[String, AnyRef]
  ): Unit = {
    this.debugContext = debugContext
  }

  override def isInEvaluation(thread: ThreadReference) = false

  override def evaluate(
      expression: String,
      thread: ThreadReference,
      depth: Int
  ): CompletableFuture[Value] = {
    val frame = thread.frames().get(depth)
    evaluator.evaluate(expression, thread, frame)(debugContext)
  }

  override def evaluate(
      expression: String,
      thisContext: ObjectReference,
      thread: ThreadReference
  ): CompletableFuture[Value] = ???

  override def evaluateForBreakpoint(
      breakpoint: IEvaluatableBreakpoint,
      thread: ThreadReference
  ): CompletableFuture[Value] = ???

  override def invokeMethod(
      thisContext: ObjectReference,
      methodName: String,
      methodSignature: String,
      args: Array[Value],
      thread: ThreadReference,
      invokeSuper: Boolean
  ): CompletableFuture[Value] = evaluator.invokeMethod(
    thisContext,
    methodName,
    methodSignature,
    args,
    thread,
    invokeSuper
  )(debugContext)

  override def clearState(thread: ThreadReference): Unit = {}
}

object NoopEvaluationProvider extends IEvaluationProvider {

  override def isInEvaluation(thread: ThreadReference) = false

  override def evaluate(
      expression: String,
      thread: ThreadReference,
      depth: Int
  ): CompletableFuture[Value] = ???

  override def evaluate(
      expression: String,
      thisContext: ObjectReference,
      thread: ThreadReference
  ): CompletableFuture[Value] = ???

  override def evaluateForBreakpoint(
      breakpoint: IEvaluatableBreakpoint,
      thread: ThreadReference
  ): CompletableFuture[Value] = ???

  override def invokeMethod(
      thisContext: ObjectReference,
      methodName: String,
      methodSignature: String,
      args: Array[Value],
      thread: ThreadReference,
      invokeSuper: Boolean
  ): CompletableFuture[Value] = ???

  override def clearState(thread: ThreadReference): Unit = {}
}
