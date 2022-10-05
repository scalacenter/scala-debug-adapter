package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.Debuggee
import ch.epfl.scala.debugadapter.DebugTools
import ch.epfl.scala.debugadapter.Logger
import com.microsoft.java.debug.core.DebugSettings
import com.microsoft.java.debug.core.adapter.{StepFilterProvider => _, _}
import com.microsoft.java.debug.core.protocol.Types
import com.sun.jdi._

import java.util
import java.util.Collections
import java.util.concurrent.CompletableFuture
import java.util.function.Consumer
import io.reactivex.Observable

private[debugadapter] object DebugAdapter {

  /**
   * Since Scala 2.13, object fields are represented by static fields in JVM byte code.
   * See https://github.com/scala/scala/pull/7270
   */
  DebugSettings.getCurrent.showStaticVariables = true

  def context(debuggee: Debuggee, tools: DebugTools, logger: Logger, testMode: Boolean): IProviderContext = {
    TimeUtils.logTime(logger, "Configured debugger") {
      val context = new ProviderContext
      val classEntries = debuggee.classEntries
      val distinctEntries = classEntries
        .groupBy(e => e.name)
        .map { case (name, group) =>
          if (group.size > 1) logger.warn(s"Found duplicate entry $name in debuggee ${debuggee.name}")
          group.head
        }
        .toSeq
      val sourceLookUpProvider = SourceLookUpProvider(distinctEntries, logger)

      context.registerProvider(classOf[IHotCodeReplaceProvider], HotCodeReplaceProvider)
      context.registerProvider(classOf[IVirtualMachineManagerProvider], VirtualMachineManagerProvider)
      context.registerProvider(classOf[ISourceLookUpProvider], sourceLookUpProvider)
      context.registerProvider(
        classOf[IEvaluationProvider],
        EvaluationProvider(debuggee, tools, sourceLookUpProvider, logger, testMode)
      )
      context.registerProvider(classOf[ICompletionsProvider], CompletionsProvider)
      context.registerProvider(
        classOf[IStepFilterProvider],
        StepFilterProvider(debuggee, tools, sourceLookUpProvider, logger, testMode)
      )
      context
    }
  }

  object CompletionsProvider extends ICompletionsProvider {
    override def codeComplete(
        frame: StackFrame,
        snippet: String,
        line: Int,
        column: Int
    ): util.List[Types.CompletionItem] = Collections.emptyList()
  }

  object HotCodeReplaceProvider extends IHotCodeReplaceProvider {
    override def onClassRedefined(consumer: Consumer[util.List[String]]): Unit =
      ()

    override def redefineClasses(): CompletableFuture[util.List[String]] =
      CompletableFuture.completedFuture(Collections.emptyList())

    override def getEventHub: Observable[HotCodeReplaceEvent] =
      Observable.empty()
  }

  object VirtualMachineManagerProvider extends IVirtualMachineManagerProvider {
    def getVirtualMachineManager: VirtualMachineManager =
      Bootstrap.virtualMachineManager
  }
}
