package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.DebuggeeRunner
import ch.epfl.scala.debugadapter.internal.evaluator.EvaluationDriver
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

private[internal] object EvaluationProvider {
  def apply(
      runner: DebuggeeRunner,
      sourceLookUpProvider: SourceLookUpProvider,
      logger: Logger,
      testMode: Boolean
  ): IEvaluationProvider = {
    val evaluator = runner.evaluationClassLoader
      .flatMap(EvaluationDriver(_))
      .map(
        new ExpressionEvaluator(runner.scalaVersion, runner.classPath, sourceLookUpProvider, _, logger, testMode)
      )
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
    val future = new CompletableFuture[Value]()
    evaluator match {
      case None =>
        future.completeExceptionally(
          new Exception("Missing evaluator for this debug session")
        )
      case Some(evaluator) =>
        val frame = thread.frames().get(depth)
        if (frame.location().sourcePath().endsWith(".java"))
          future.completeExceptionally(
            new UnsupportedOperationException(
              "Evaluation in Java source file not supported"
            )
          )
        else
          evaluationBlock {
            val evaluation = evaluator.evaluate(expression, thread, depth)
            completeFuture(future, evaluation)
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
      val invocation = obj
        .invoke(
          methodName,
          methodSignature,
          if (args == null) List() else args.toList
        )
        .recover {
          // if invocation throws an exception, we return that exception as the result
          case MethodInvocationFailed(msg, exception) => exception
        }
      completeFuture(future, invocation.getResult)
      debugContext.getStackFrameManager.reloadStackFrames(thread)
    }
    future
  }

  private def completeFuture[T](future: CompletableFuture[T], result: Try[T]): Unit = {
    result match {
      case Success(value) =>
        future.complete(value)
      case Failure(exception) =>
        future.completeExceptionally(exception)
    }
  }

  private def evaluationBlock(f: => Unit): Unit = {
    isEvaluating.set(true)
    try f
    finally { isEvaluating.set(false) }
  }

  override def clearState(thread: ThreadReference): Unit = {}
}
