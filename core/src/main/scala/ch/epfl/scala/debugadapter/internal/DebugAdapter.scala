package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.{DebuggeeRunner, Logger}
import com.microsoft.java.debug.core.adapter._
import com.microsoft.java.debug.core.protocol.Types
import com.microsoft.java.debug.core.{DebugSettings, IEvaluatableBreakpoint}
import com.sun.jdi._
import io.reactivex.Observable

import java.util
import java.util.Collections
import java.util.concurrent.CompletableFuture
import java.util.function.Consumer

private[debugadapter] object DebugAdapter {
  /**
   * Disable evaluation of variable's `toString` methods
   * since code evaluation is not supported.
   *
   * Debug adapter, when asked for variables, tries to present them in a readable way,
   * hence it evaluates the `toString` method for each object providing it.
   * The adapter is not checking if evaluation is supported, so the whole request
   * fails if there is at least one variable with custom `toString` in scope.
   *
   * See usages of [[com.microsoft.java.debug.core.adapter.variables.VariableDetailUtils.formatDetailsValue()]]
   */
  DebugSettings.getCurrent.showToString = false

    /**
    * Since Scala 2.13, object fields are represented by static fields in JVM byte code.
    * See https://github.com/scala/scala/pull/7270
    */
  DebugSettings.getCurrent.showStaticVariables = true

  def context(runner: DebuggeeRunner, logger: Logger): IProviderContext = {
    val context = new ProviderContext
    context.registerProvider(classOf[IHotCodeReplaceProvider], HotCodeReplaceProvider)
    context.registerProvider(classOf[IVirtualMachineManagerProvider], VirtualMachineManagerProvider)
    context.registerProvider(classOf[ISourceLookUpProvider], SourceLookUpProvider(runner.classPathEntries, logger))
    context.registerProvider(classOf[IEvaluationProvider], EvaluationProvider)
    context.registerProvider(classOf[ICompletionsProvider], CompletionsProvider)
    context
  }

  object CompletionsProvider extends ICompletionsProvider {
    override def codeComplete(
        frame: StackFrame,
        snippet: String,
        line: Int,
        column: Int
    ): util.List[Types.CompletionItem] = Collections.emptyList()
  }

  object EvaluationProvider extends IEvaluationProvider {
    override def isInEvaluation(thread: ThreadReference): Boolean = false

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

  object HotCodeReplaceProvider extends IHotCodeReplaceProvider {
    override def onClassRedefined(consumer: Consumer[util.List[String]]): Unit = ()
    override def redefineClasses(): CompletableFuture[util.List[String]] =
      CompletableFuture.completedFuture(Collections.emptyList())
    override def getEventHub: Observable[HotCodeReplaceEvent] = Observable.empty()
  }

  object VirtualMachineManagerProvider extends IVirtualMachineManagerProvider {
    def getVirtualMachineManager: VirtualMachineManager = Bootstrap.virtualMachineManager
  }
}
