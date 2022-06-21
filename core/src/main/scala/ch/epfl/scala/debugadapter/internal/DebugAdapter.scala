package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.{DebuggeeRunner, Logger}
import com.microsoft.java.debug.core.DebugSettings
import com.microsoft.java.debug.core.adapter._
import com.microsoft.java.debug.core.protocol.Types
import com.sun.jdi._

import java.util
import java.util.Collections
import java.util.concurrent.CompletableFuture
import java.util.function.Consumer
import com.microsoft.java.debug.core.protocol.Requests.CustomStepFilter
import ch.epfl.scala.debugadapter.internal.decompiler.scalasig._
import ch.epfl.scala.debugadapter.internal.decompiler.scalasig
import scala.collection.JavaConverters._
import io.reactivex.Observable

private[debugadapter] object DebugAdapter {

  /**
   * Since Scala 2.13, object fields are represented by static fields in JVM byte code.
   * See https://github.com/scala/scala/pull/7270
   */
  DebugSettings.getCurrent.showStaticVariables = true

  def context(runner: DebuggeeRunner, logger: Logger): IProviderContext = {
    TimeUtils.logTime(logger, "Configured debugger") {
      val context = new ProviderContext
      val sourceLookUpProvider = SourceLookUpProvider(
        runner.classPathEntries ++ runner.javaRuntime,
        logger
      )
      DebugSettings.getCurrent().stepFilters.customStepFilter =
        new CustomStepFilter {
          override def skip(method: Method): Boolean = {
            // Check wether the signature looks like a lambda
            if (isLocalMethod(method) || isLocalClass(method.declaringType()))
              false
            else {
              val fqcn = method.declaringType().name()
              val res =
                sourceLookUpProvider.getScalaSig(fqcn).map(skip(method, _))

              res match {
                case None => println(s"No ScalaSig found for $method")
                case Some(true) => println(s"Skipping $method")
                case Some(false) => ()
              }

              res.getOrElse(false)

            }
          }

          private def isLocalMethod(method: Method): Boolean =
            method.name().contains("$anonfun$")

          private def isLocalClass(tpe: ReferenceType): Boolean =
            tpe.name().contains("$anon$")

            private def skip(method: Method, scalaSig: ScalaSig): Boolean = {
            scalaSig.entries
              .collect { case m: MethodSymbol => m }
              .forall(!matchSymbol(method, _))
          }

          private def matchSymbol(
              javaMethod: Method,
              scalaMethod: MethodSymbol
          ): Boolean = {
            // println(s"name: ${scalaMethod.name}")
            if (scalaMethod.aliasRef.nonEmpty)
              println(
                s"aliasRef for ${scalaMethod.name}: ${scalaMethod.aliasRef}"
              )
            // if (scalaMethod.attributes.nonEmpty) println(s"attributes for ${scalaMethod.name}: ${scalaMethod.attributes}")
            if (scalaMethod.isSyntheticMethod)
              println(s"${scalaMethod.name} isSyntheticMethod")
            if (scalaMethod.isMonomorphic)
              println(s"${scalaMethod.name} isMonomorphic")
            if (scalaMethod.isMixedIn) println(s"${scalaMethod.name} isMixedIn")
            // println(s"MethodType: $methodType")
            javaMethod.name == scalaMethod.name &&
            matchArguments(javaMethod, scalaMethod.info.info.get) &&
            matchOwner(javaMethod.declaringType(), scalaMethod.parent.get)
          }

          private def matchOwner(
              javaClass: ReferenceType,
              scalaOwner: Symbol
          ): Boolean = {
            // println(s"matchOwner(${javaClass.name()}, ${scalaOwner.name})")
            val fqcn = javaClass.name()
            // TODO improve
            getOwners(scalaOwner).reverse
              .foldLeft(Option(fqcn)) { (acc, sym) =>
                for (fqcn <- acc if fqcn.contains(sym.name)) yield {
                  fqcn
                    .split(sym.name)
                    .drop(1)
                    .mkString(sym.name)
                }
              }
              .exists { remainder =>
                remainder.forall(c => c.isDigit || c == '$')
              }
          }

          private def getOwners(sym: Symbol): Seq[Symbol] = {
            Iterator
              .iterate(Option(sym))(opt => opt.flatMap(_.parent))
              .takeWhile(_.isDefined)
              .flatten
              .toSeq
          }

          private def matchArguments(
              javaMethod: Method,
              methodType: scalasig.Type
          ): Boolean = {
            val javaArgs = javaMethod.arguments().asScala.toSeq
            val scalaArgs = extractArguments(methodType)
            javaArgs.size == scalaArgs.size &&
            javaArgs.zip(scalaArgs).forall { case (javaArg, scalaArg) =>
              matchArgument(javaArg, scalaArg)
            }
          }

            private def matchArgument(
              javaArg: LocalVariable,
              scalaArg: Symbol
          ): Boolean = {
            // println(scalaArg)
            javaArg.name() == scalaArg.name
          }

              // private def matchType(javaType: jdi.Type)

          private def extractArguments(methodType: scalasig.Type): Seq[Symbol] = {
            methodType match {
              case m: MethodType => m.paramRefs
              case m: NullaryMethodType => Seq.empty
              case m: PolyType => extractArguments(m.typeRef.get)
            }
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
        EvaluationProvider(runner, sourceLookUpProvider, logger)
      )
      context.registerProvider(classOf[ICompletionsProvider], CompletionsProvider)
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
