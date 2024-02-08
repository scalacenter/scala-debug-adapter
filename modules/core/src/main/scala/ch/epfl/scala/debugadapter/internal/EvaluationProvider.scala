package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.BuildInfo
import ch.epfl.scala.debugadapter.ClassEntry
import ch.epfl.scala.debugadapter.DebugConfig
import ch.epfl.scala.debugadapter.Debuggee
import ch.epfl.scala.debugadapter.EvaluationFailed
import ch.epfl.scala.debugadapter.JavaRuntime
import ch.epfl.scala.debugadapter.Logger
import ch.epfl.scala.debugadapter.ManagedEntry
import ch.epfl.scala.debugadapter.internal.evaluator.*
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
    compilers: Map[ClassEntry, ExpressionCompiler],
    evaluationMode: DebugConfig.EvaluationMode,
    testMode: Boolean,
    logger: Logger
) extends IEvaluationProvider {

  private var debugContext: IDebugAdapterContext = _
  private val isEvaluating = new AtomicBoolean(false)
  private val runtimeEvaluator = new RuntimeEvaluator(sourceLookUp, logger)

  override def initialize(debugContext: IDebugAdapterContext, options: java.util.Map[String, AnyRef]): Unit =
    this.debugContext = debugContext

  override def isInEvaluation(thread: ThreadReference) = isEvaluating.get

  override def evaluate(expression: String, thread: ThreadReference, depth: Int): CompletableFuture[Value] = {
    val frame = JdiFrame(thread, depth)
    val evaluation = for {
      preparedExpression <- prepare(expression, frame, preEvaluation = true)
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
    val frame = JdiFrame(thread, 0)
    val location = frame.current().location
    val locationCode = (location.method.name, location.codeIndex).hashCode
    val expression =
      if (breakpoint.getCompiledExpression(locationCode) != null)
        breakpoint.getCompiledExpression(locationCode).asInstanceOf[Try[PreparedExpression]]
      else if (breakpoint.containsConditionalExpression)
        prepare(breakpoint.getCondition, frame, preEvaluation = false)
      else if (breakpoint.containsLogpointExpression)
        prepareLogMessage(breakpoint.getLogMessage, frame)
      else Failure(new Exception("Missing expression"))
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
    val obj = JdiObject(thisContext, thread)
    val wrappedArgs = if (args == null) Seq.empty else args.toSeq.map(JdiValue(_, thread))
    val invocation = evaluationBlock {
      obj
        .invoke(methodName, methodSignature, wrappedArgs)
        .recover {
          // if invocation throws an exception, we return that exception as the result
          case RuntimeException(msg, Some(exception)) => exception
        }
        .map(_.value)
    }
    completeFuture(invocation.getResult, thread)
  }

  private def prepareLogMessage(message: String, frame: JdiFrame): Try[PreparedExpression] = {
    if (!message.contains("$")) {
      Success(PlainLogMessage(message))
    } else if (evaluationMode.allowScalaEvaluation) {
      val tripleQuote = "\"\"\""
      val expression = s"""println(s$tripleQuote$message$tripleQuote)"""
      getScalaEvaluator(frame).flatMap(_.compile(expression))
    } else Failure(new EvaluationFailed(s"Cannot evaluate logpoint '$message' with $evaluationMode mode"))
  }

  private def prepare(expression: String, frame: JdiFrame, preEvaluation: Boolean): Try[PreparedExpression] = {
    val scalaEvaluator = getScalaEvaluator(frame)
    def compiledExpression = scalaEvaluator.flatMap(_.compile(expression))
    if (evaluationMode.allowRuntimeEvaluation) {
      runtimeEvaluator.validate(expression, frame, preEvaluation) match {
        case Success(expr) if scalaEvaluator.isSuccess && containsMethodCall(expr.tree) =>
          Success(compiledExpression.getOrElse(expr))
        case success: Success[RuntimeExpression] => success
        case failure: Failure[RuntimeExpression] =>
          if (scalaEvaluator.isSuccess) compiledExpression
          else failure
      }
    } else if (evaluationMode.allowScalaEvaluation) compiledExpression
    else Failure(new EvaluationFailed(s"Evaluation is disabled"))
  }

  private def getScalaEvaluator(frame: JdiFrame): Try[ScalaEvaluator] =
    if (evaluationMode.allowScalaEvaluation)
      for {
        fqcn <- Try(frame.current().location.declaringType.name)
        entry <- sourceLookUp.getClassEntry(fqcn).toTry(s"Unknown class $fqcn")
        compiler <- compilers.get(entry).toTry(missingCompilerMessage(entry))
        sourceContent <- sourceLookUp
          .getSourceContentFromClassName(fqcn)
          .toTry(s"Cannot find source file of class $fqcn")
      } yield new ScalaEvaluator(sourceContent, frame, compiler, logger, testMode)
    else Failure(new Exception("Scala evaluation is not allowed"))

  private def missingCompilerMessage(entry: ClassEntry): String =
    entry match {
      case m: ManagedEntry =>
        m.scalaVersion match {
          case None =>
            s"Failed resolving scala-expression-compiler:${BuildInfo.version} for ${entry.name} (Missing Scala Version)"
          case Some(sv) =>
            s"""|Failed resolving scala-expression-compiler:${BuildInfo.version} for Scala $sv.
                |Please open an issue at https://github.com/scalacenter/scala-debug-adapter.""".stripMargin
        }
      case _: JavaRuntime =>
        s"Failed resolving scala-expression-compiler:${BuildInfo.version} for ${entry.name} (Missing Scala Version)"
      case _ =>
        s"Failed resolving scala-expression-compiler:${BuildInfo.version} for ${entry.name} (Unknown Scala Version)"
    }

  private def containsMethodCall(tree: RuntimeEvaluationTree): Boolean = {
    import RuntimeEvaluationTree.*
    tree match {
      case NestedModule(_, init) => containsMethodCall(init.qualifier)
      case InstanceField(_, qualifier) => containsMethodCall(qualifier)
      case If(p, t, f, _) => containsMethodCall(p) || containsMethodCall(t) || containsMethodCall(f)
      case Assign(lhs, rhs, _) => containsMethodCall(lhs) || containsMethodCall(rhs)
      case _: CallMethod | _: NewInstance => true
      case _: LocalVar | _: RuntimeEvaluationTree.Value | _: This => false
      case _: StaticField | _: StaticModule => false
      case _: CallBinaryOp | _: CallUnaryOp | _: ArrayElem => false
    }
  }

  private def evaluate(expression: PreparedExpression, frame: JdiFrame): Try[Value] = evaluationBlock {
    val result = expression match {
      case logMessage: PlainLogMessage => MessageLogger.log(logMessage, frame)
      case expr: RuntimeExpression => runtimeEvaluator.evaluate(expr, frame)
      case expr: CompiledExpression =>
        for {
          scalaEvaluator <- getScalaEvaluator(frame)
          compiledExpression <- scalaEvaluator.evaluate(expr)
        } yield compiledExpression
    }
    // if evaluation throws an exception, we return that exception as the result
    result.recover { case RuntimeException(_, Some(exception)) => exception.value }
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
    finally isEvaluating.set(false)
  }

  override def clearState(thread: ThreadReference): Unit = {}
}

private[internal] object EvaluationProvider {
  def apply(debuggee: Debuggee, tools: DebugTools, logger: Logger, config: DebugConfig): IEvaluationProvider =
    new EvaluationProvider(
      tools.sourceLookUp,
      tools.expressionCompilers,
      config.evaluationMode,
      config.testMode,
      logger
    )
}
