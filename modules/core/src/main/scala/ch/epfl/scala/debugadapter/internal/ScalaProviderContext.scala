package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.DebugConfig
import ch.epfl.scala.debugadapter.Debuggee
import ch.epfl.scala.debugadapter.Logger
import com.microsoft.java.debug.core.DebugSettings
import com.microsoft.java.debug.core.adapter.ICompletionsProvider
import com.microsoft.java.debug.core.adapter.IEvaluationProvider
import com.microsoft.java.debug.core.adapter.IHotCodeReplaceProvider
import com.microsoft.java.debug.core.adapter.ISourceLookUpProvider
import com.microsoft.java.debug.core.adapter.IStackTraceProvider
import com.microsoft.java.debug.core.adapter.IVirtualMachineManagerProvider
import com.microsoft.java.debug.core.adapter.ProviderContext
import com.microsoft.java.debug.core.adapter.variables.IVariableProvider
import ch.epfl.scala.debugadapter.internal.stacktrace.StepFilter

private[debugadapter] class ScalaProviderContext private (
    debuggee: Debuggee,
    logger: Logger,
    config: DebugConfig
) extends ProviderContext {
  def configure(tools: DebugTools, stepFilters: Seq[StepFilter]): Unit = {
    registerProvider(classOf[ISourceLookUpProvider], tools.sourceLookUp)
    registerProvider(classOf[IEvaluationProvider], EvaluationProvider(debuggee, tools, logger, config))
    registerProvider(
      classOf[IStackTraceProvider],
      StackTraceProvider(debuggee, tools, logger, config.testMode, stepFilters)
    )
  }
}

private[debugadapter] object ScalaProviderContext {
  def apply(
      debuggee: Debuggee,
      logger: Logger,
      config: DebugConfig
  ): ScalaProviderContext = {

    /**
     * Since Scala 2.13, object fields are represented by static fields in JVM byte code.
     * See https://github.com/scala/scala/pull/7270
     */
    DebugSettings.getCurrent.showStaticVariables = true

    val context = new ScalaProviderContext(debuggee, logger, config)
    val hotCodeReplaceProvider = HotCodeReplaceProvider(debuggee, logger, config.testMode)
    // The BreakpointRequestHandler resolves the IHotCodeReplaceProvider in its constructor
    context.registerProvider(classOf[IHotCodeReplaceProvider], hotCodeReplaceProvider)
    context.registerProvider(classOf[ICompletionsProvider], CompletionsProvider)
    context.registerProvider(classOf[IVirtualMachineManagerProvider], VirtualMachineManagerProvider)
    context.registerProvider(classOf[IVariableProvider], VariableProvider)
    context
  }
}
