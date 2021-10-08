package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.DebuggeeRunner
import com.microsoft.java.debug.core.IEvaluatableBreakpoint
import com.microsoft.java.debug.core.adapter.{
  IDebugAdapterContext,
  IEvaluationProvider
}
import com.sun.jdi.{ObjectReference, ThreadReference, Value}

import java.util.concurrent.CompletableFuture
import ch.epfl.scala.debugadapter.internal.evaluator.{
  ExpressionEvaluator,
  ExpressionCompiler
}
import ch.epfl.scala.debugadapter.internal.evaluator.JdiObject
import scala.util.Failure
import scala.util.Success
import java.util.concurrent.atomic.AtomicBoolean

private[internal] object EvaluationProvider {
  def apply(
      runner: DebuggeeRunner,
      sourceLookUpProvider: SourceLookUpProvider
  ): IEvaluationProvider = {
    val evaluator = runner.evaluationClassLoader
      .flatMap(ExpressionCompiler(_))
      .map(new ExpressionEvaluator(sourceLookUpProvider, _))
    new EvaluationProvider(evaluator)
  }
}

private[internal] class EvaluationProvider(
    evaluator: Option[ExpressionEvaluator]
) extends IEvaluationProvider {

  private var debugContext: IDebugAdapterContext = _
  private val isEvaluating = new AtomicBoolean(false)

  override def initialize(
      debugContext: IDebugAdapterContext,
      options: java.util.Map[String, AnyRef]
  ): Unit = {
    this.debugContext = debugContext
  }

  override def isInEvaluation(thread: ThreadReference) = isEvaluating.get()

  override def evaluate(
      expression: String,
      thread: ThreadReference,
      depth: Int
  ): CompletableFuture[Value] = {
    val frame = thread.frames().get(depth)
    val future = new CompletableFuture[Value]()
    evaluator match {
      case None =>
        future.completeExceptionally(
          new Exception("Missing evaluator for this debug session")
        )
      case Some(evaluator) =>
        evaluationBlock {
          evaluator.evaluate(expression, thread, frame) match {
            case Failure(exception) =>
              future.completeExceptionally(exception)
            case Success(value) =>
              future.complete(value)
          }
        }
    }
    debugContext.getStackFrameManager.reloadStackFrames(thread)
    future
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
  ): CompletableFuture[Value] = {
    val future = new CompletableFuture[Value]()
    val obj = new JdiObject(thisContext, thread)
    evaluationBlock {
      val invocation = obj.invoke(
        methodName,
        methodSignature,
        if (args == null) List() else args.toList
      )
      invocation.getResult match {
        case Success(value) =>
          future.complete(value)
        case Failure(exception) =>
          future.completeExceptionally(exception)
      }
      debugContext.getStackFrameManager.reloadStackFrames(thread)
    }
    future
  }

  private def evaluationBlock(f: => Unit): Unit = {
    isEvaluating.set(true)
    try f
    finally { isEvaluating.set(false) }
  }

  override def clearState(thread: ThreadReference): Unit = {}
}
