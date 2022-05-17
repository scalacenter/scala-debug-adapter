package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.{DebuggeeRunner, Logger}
import com.microsoft.java.debug.core.DebugSettings
import com.microsoft.java.debug.core.adapter._
import com.microsoft.java.debug.core.protocol.Types
import com.sun.jdi._
import io.reactivex.Observable

import java.util
import java.util.Collections
import java.util.concurrent.CompletableFuture
import java.util.function.Consumer
import com.microsoft.java.debug.core.protocol.Requests.CustomStepFilter
import ch.epfl.scala.debugadapter.internal.decompiler.Decompiler
import ch.epfl.scala.debugadapter.internal.decompiler.scalasig.ScalaSig

private[debugadapter] object DebugAdapter {

  /**
   * Since Scala 2.13, object fields are represented by static fields in JVM byte code.
   * See https://github.com/scala/scala/pull/7270
   */
  DebugSettings.getCurrent.showStaticVariables = true

  def context(runner: DebuggeeRunner, logger: Logger): IProviderContext = {
    val context = new ProviderContext
    val sourceLookUpProvider = SourceLookUpProvider(
      runner.classPathEntries ++ runner.javaRuntime
    )
    DebugSettings.getCurrent().stepFilters.customStepFilter =
      new CustomStepFilter {
        override def skip(method: Method): Boolean = {
          // TODO: Check name, return type, arguments number and types
          // If sure => skip
          // Else step in
          println(method.signature())

          // Check wether the signature looks like a lambda
          if (method.name().contains("%anonfunc%")) {
            false
          }

          val className = method.getClass().getCanonicalName()

          // How to access fqcnToClassPathEntry?
          val classPathEntry = sourceLookUpProvider

          val bytes: Array[Byte] =
            ??? // sourceLookUpProvider.getBytes(className)

          val optScalaSig =
            ??? // Decompiler.decompileMethodSymbol(bytes, className)
          if (optScalaSig == None) {
            false
          }

          val scalaSig: ScalaSig = ??? // optScalaSig.get

          // TODO Checks

          true
        }
      }

    context.registerProvider(
      classOf[IHotCodeReplaceProvider],
      HotCodeReplaceProvider
    )
    context.registerProvider(
      classOf[IVirtualMachineManagerProvider],
      VirtualMachineManagerProvider
    )
    context.registerProvider(
      classOf[ISourceLookUpProvider],
      sourceLookUpProvider
    )
    context.registerProvider(
      classOf[IEvaluationProvider],
      EvaluationProvider(runner, sourceLookUpProvider)
    )
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
