package ch.epfl.scala.debugadapter.internal.stepfilter

import com.sun.jdi.Method
import com.sun.jdi.ReferenceType
import com.sun.jdi.AbsentInformationException
import ch.epfl.scala.debugadapter.internal.ByteCodes
import ch.epfl.scala.debugadapter.internal.SourceLookUpProvider
import ch.epfl.scala.debugadapter.Debuggee
import ch.epfl.scala.debugadapter.DebugTools
import ch.epfl.scala.debugadapter.Logger
import scala.jdk.CollectionConverters.*
import ch.epfl.scala.debugadapter.internal.ScalaExtension.*

trait ScalaStepFilter extends StepFilter {
  protected def skipScalaMethod(method: Method): Boolean

  override def shouldSkipOver(method: Method): Boolean = {
    if (method.isBridge) true
    else if (isDynamicClass(method.declaringType)) true
    else if (isJava(method)) false
    else if (isConstructor(method)) false
    else if (isLocalMethod(method))
      !isLazyInitializer(method) && isLazyGetter(method)
    else if (isAnonFunction(method)) false
    else if (isLocalClass(method.declaringType)) false
    else if (isDefaultValue(method)) false
    else if (isTraitInitializer(method)) skipTraitInitializer(method)
    else skipScalaMethod(method)
  }

  private def isDynamicClass(tpe: ReferenceType): Boolean =
    try {
      // source of java.lang.invoke.LambdaForm$DMH.1175962212.invokeStatic_L_L(java.lang.Object, java.lang.Object) is LambdaForm$DMH
      !tpe.sourceName.contains('.')
    } catch {
      case _: AbsentInformationException =>
        // We assume that a ReferenceType with no source name is necessarily a dynamic class
        true
    }

  private def isJava(method: Method): Boolean =
    method.declaringType.sourceName.endsWith(".java")

  private def isConstructor(method: Method): Boolean =
    method.name == "<init>"

  private def isLocalMethod(method: Method): Boolean =
    method.name.matches(".+\\$\\d+")

  private def isLazyInitializer(method: Method): Boolean =
    method.name.contains("$lzyINIT") || method.name.contains("$lzycompute$")

  private val lazyTypes: Set[String] = Set(
    "scala.runtime.LazyRef",
    "scala.runtime.LazyBoolean",
    "scala.runtime.LazyByte",
    "scala.runtime.LazyChar",
    "scala.runtime.LazyShort",
    "scala.runtime.LazyInt",
    "scala.runtime.LazyLong",
    "scala.runtime.LazyFloat",
    "scala.runtime.LazyDouble",
    "scala.runtime.LazyUnit"
  )
  private def isLazyGetter(method: Method): Boolean =
    method.argumentTypes.asScala.toSeq match {
      case Seq(argType) => lazyTypes.contains(argType.name)
      case _ => false
    }

  private def isAnonFunction(method: Method): Boolean =
    method.name.contains("$anonfun$")

  private def isDefaultValue(method: Method): Boolean =
    method.name.contains("$default$")

  private def isLocalClass(tpe: ReferenceType): Boolean =
    tpe.name.contains("$anon$")

  private def isTraitInitializer(method: Method): Boolean =
    method.name == "$init$"

  private def skipTraitInitializer(method: Method): Boolean =
    method.bytecodes.toSeq == Seq(ByteCodes.RETURN)
}

object ScalaStepFilter {
  def apply(
      debuggee: Debuggee,
      tools: DebugTools,
      sourceLookUp: SourceLookUpProvider,
      logger: Logger,
      testMode: Boolean
  ): StepFilter = {
    if (debuggee.scalaVersion.isScala2)
      new Scala2StepFilter(sourceLookUp, debuggee.scalaVersion, logger, testMode)
    else
      tools.stepFilter
        .flatMap { classLoader =>
          Scala3StepFilter
            .tryLoad(debuggee, classLoader, logger, testMode)
            .warnFailure(logger, s"Cannot load step filter for Scala ${debuggee.scalaVersion}")
        }
        .getOrElse(fallback)
  }

  private def fallback: StepFilter = new ScalaStepFilter {
    override protected def skipScalaMethod(method: Method): Boolean = false
  }
}
