package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.BuildInfo
import ch.epfl.scala.debugadapter.ClassEntry
import ch.epfl.scala.debugadapter.DebugConfig
import ch.epfl.scala.debugadapter.Debuggee
import ch.epfl.scala.debugadapter.EvaluationFailed
import ch.epfl.scala.debugadapter.JavaRuntime
import ch.epfl.scala.debugadapter.Logger
import ch.epfl.scala.debugadapter.ManagedEntry
import ch.epfl.scala.debugadapter.UnmanagedEntry
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
    messageLogger: MessageLogger,
    scalaEvaluators: Map[ClassEntry, ScalaEvaluator],
    mode: DebugConfig.EvaluationMode,
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
      if (breakpoint.getCompiledExpression(locationCode) != null) {
        breakpoint.getCompiledExpression(locationCode).asInstanceOf[Try[PreparedExpression]]
      } else if (breakpoint.containsConditionalExpression) {
        prepare(breakpoint.getCondition, frame, preEvaluation = false)
      } else if (breakpoint.containsLogpointExpression) {
        prepareLogMessage(breakpoint.getLogMessage, frame)
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
    val obj = JdiObject(thisContext, thread)
    val wrappedArgs = if (args == null) Seq.empty else args.toSeq.map(JdiValue(_, thread))
    val invocation = evaluationBlock {
      obj
        .invoke(methodName, methodSignature, wrappedArgs)
        .recover {
          // if invocation throws an exception, we return that exception as the result
          case MethodInvocationFailed(msg, Some(exception)) => exception
        }
        .map(_.value)
    }
    completeFuture(invocation.getResult, thread)
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
            s"""|Missing scala-expression-compiler_$sv with version ${BuildInfo.version}.
                |You can open an issue at https://github.com/scalacenter/scala-debug-adapter.""".stripMargin
        }
      case _: JavaRuntime => s"Unsupported evaluation in JDK: ${entry.name}"
      case _: UnmanagedEntry => s"Unsupported evaluation in unmanaged classpath entry: ${entry.name}"
      case _ => s"Unsupported evaluation in ${entry.name}"
    }

  private def prepareLogMessage(message: String, frame: JdiFrame): Try[PreparedExpression] = {
    if (!message.contains("$")) {
      Success(PlainLogMessage(message))
    } else if (mode.allowScalaEvaluation) {
      val tripleQuote = "\"\"\""
      val expression = s"""println(s$tripleQuote$message$tripleQuote)"""
      compile(expression, frame)
    } else Failure(new EvaluationFailed(s"Cannot evaluate logpoint '$message' with $mode mode"))
  }

  private def prepare(expression: String, frame: JdiFrame, preEvaluation: Boolean): Try[PreparedExpression] =
    if (mode.allowRuntimeEvaluation)
      runtimeEvaluator.validate(expression, frame, preEvaluation) match {
        case Success(expr) if mode.allowScalaEvaluation && containsMethodCall(expr.tree) =>
          compile(expression, frame).orElse(Success(expr))
        case success: Success[RuntimeExpression] => success
        case failure: Failure[RuntimeExpression] =>
          if (mode.allowScalaEvaluation) compile(expression, frame) else failure
      }
    else if (mode.allowScalaEvaluation) compile(expression, frame)
    else Failure(new EvaluationFailed(s"Evaluation is disabled"))

  private def compile(expression: String, frame: JdiFrame): Try[CompiledExpression] = {
    val fqcn = frame.current().location.declaringType.name
    for {
      evaluator <- getScalaEvaluator(fqcn)
      sourceContent <- sourceLookUp
        .getSourceContentFromClassName(fqcn)
        .toTry(s"Cannot find source file of class $fqcn")
      preparedExpression <- evaluator.compile(sourceContent, expression, frame)
    } yield preparedExpression
  }

  private def containsMethodCall(tree: RuntimeEvaluableTree): Boolean = {
    tree match {
      case mt: NestedModuleTree => containsMethodCall(mt.init.qual)
      case ft: InstanceFieldTree => containsMethodCall(ft.qual)
      case IfTree(p, t, f, _) => containsMethodCall(p) || containsMethodCall(t) || containsMethodCall(f)
      case AssignTree(lhs, rhs, _) => containsMethodCall(lhs) || containsMethodCall(rhs)
      case _: MethodTree | _: NewInstanceTree => true
      case _: LocalVarTree | _: RuntimeValueTree | _: ThisTree => false
      case _: StaticFieldTree | _: TopLevelModuleTree => false
      case _: BinaryOpTree | _: UnaryOpTree | _: ArrayElemTree => false
    }
  }

  private def evaluate(expression: PreparedExpression, frame: JdiFrame): Try[Value] = evaluationBlock {
    expression match {
      case logMessage: PlainLogMessage => messageLogger.log(logMessage, frame)
      case expr: RuntimeExpression => runtimeEvaluator.evaluate(expr, frame)
      case expr: CompiledExpression =>
        val fqcn = frame.current().location.declaringType.name
        for {
          scalaEvaluator <- getScalaEvaluator(fqcn)
          compiledExpression <- scalaEvaluator.evaluate(expr, frame)
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
    finally isEvaluating.set(false)
  }

  override def clearState(thread: ThreadReference): Unit = {}
}

private[internal] object EvaluationProvider {
  def apply(
      debuggee: Debuggee,
      tools: DebugTools,
      logger: Logger,
      config: DebugConfig
  ): IEvaluationProvider = {
    val scalaEvaluators = tools.expressionCompilers.view.map { case (entry, compiler) =>
      (entry, new ScalaEvaluator(entry, compiler, logger, config.testMode))
    }.toMap
    val messageLogger = new MessageLogger()
    new EvaluationProvider(
      tools.sourceLookUp,
      messageLogger,
      scalaEvaluators,
      config.evaluationMode,
      logger
    )
  }
}
