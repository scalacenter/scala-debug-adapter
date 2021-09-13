package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.DebuggeeRunner
import com.microsoft.java.debug.core.IEvaluatableBreakpoint
import com.microsoft.java.debug.core.adapter.{
  IDebugAdapterContext,
  IEvaluationProvider
}
import com.sun.jdi.{ObjectReference, ThreadReference, Value}

import java.util
import java.util.concurrent.CompletableFuture
import scala.util.Try
import ch.epfl.scala.debugadapter.internal.evaluator.{
  ExpressionEvaluator,
  ExpressionCompiler
}
import ch.epfl.scala.debugadapter.internal.evaluator.JdiObject

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
    evaluator.fold(???)(_.evaluate(expression, thread, frame)(debugContext))
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
    try {
      val result = for {
        obj <- Try(new JdiObject(thisContext, thread)).toOption
        result <- obj.invoke(
          methodName,
          methodSignature,
          if (args == null) List() else args.toList
        )
      } yield result

      result match {
        case Some(value) =>
          CompletableFuture.completedFuture(value)
        case None =>
          throw new Exception(
            s"Unable to invoke $methodName on ${thisContext.referenceType.name}"
          )
      }
    } finally {
      debugContext.getStackFrameManager.reloadStackFrames(thread)
    }
  }

  override def clearState(thread: ThreadReference): Unit = {}
}
