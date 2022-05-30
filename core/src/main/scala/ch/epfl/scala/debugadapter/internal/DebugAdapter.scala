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
import ch.epfl.scala.debugadapter.internal.decompiler.scalasig.MethodType
import ch.epfl.scala.debugadapter.internal.decompiler.scalasig.MethodSymbol

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
          
          val sig = method.signature()

          // Check wether the signature looks like a lambda
          if (method.name().contains("$anonfun$")) {
            false
          }
          
          // Get the name of the class
          val className = method.getClass().getName()
          println("getName: " + className)
          println("canonical: " + method.getClass().getCanonicalName())
          Console.flush()

          val res: Option[Boolean] = for {
            classFile <- sourceLookUpProvider.getClassFile(className)
            bytes = classFile.getBytes()
            scalaSig <- Decompiler.decompileMethodSymbol(bytes, className)
          } yield skip(method, scalaSig)

          res.getOrElse(true)
        }

        def skip(method: Method, scalaSig: ScalaSig): Boolean = {
          scalaSig.entries.find{
            case m : MethodSymbol => {
              println("method name: " + method.name()) 
              println("mSymbol name: " + m.info.name.get)

              // Check names
              print("names " + (method.name() == m.info.name.get))
              
              
              val methodAttrType = method.argumentTypes()
              
              val symbolAttr = m.attributes

              // Check return type
              val methodRetType = method.returnType()
              val symRetType = m.info
              
              // Check number of args
              print("Attr num " + (methodAttrType.size == symbolAttr.size))
              
              // Check attribute type
              print("Attr type " + (symbolAttr.foldLeft(true)((cond, a) => cond && methodAttrType.contains(a.symbol.name))))

              false
              
            }
            // Similar vut with MethodType
            // case m: MethodType => 
            //   m.paramRefs.foreach(r => r.get.attributes.foreach(a => a.infoRef.get))

            //   print(m.resultType.get)
              
            //   false

            case _ => false
          }

          false
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
