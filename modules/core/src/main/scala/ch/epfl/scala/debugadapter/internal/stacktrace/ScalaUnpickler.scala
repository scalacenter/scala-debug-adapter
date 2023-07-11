package ch.epfl.scala.debugadapter.internal.stacktrace

import ch.epfl.scala.debugadapter.Debuggee
import ch.epfl.scala.debugadapter.Logger
import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.internal.ByteCodes
import ch.epfl.scala.debugadapter.internal.DebugTools
import ch.epfl.scala.debugadapter.internal.ScalaExtension.*
import com.sun.jdi.AbsentInformationException
import com.sun.jdi.Method
import com.sun.jdi.ReferenceType

import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

abstract class ScalaUnpickler(scalaVersion: ScalaVersion, testMode: Boolean) extends StepFilter {
  protected def skipScala(method: Method): Boolean
  protected def formatScala(method: Method): Option[String] = Some(formatJava(method))

  private def throwOrWarn(exception: Throwable): Unit =
    if (testMode) throw exception
    else exception.getMessage

  override def shouldSkipOver(method: Method): Boolean = {
    if (method.isBridge) true
    else if (isDynamicClass(method.declaringType)) true
    else if (isJava(method)) false
    else if (isConstructor(method)) false
    else if (isStaticConstructor(method)) false
    else if (isAdaptedMethod(method)) true
    else if (isAnonFunction(method)) false
    else if (isLiftedMethod(method)) !isLazyInitializer(method) && isLazyGetter(method)
    else if (isAnonClass(method.declaringType)) false
    // TODO in Scala 3 we should be able to find the symbol of a local class using TASTy Query
    else if (isLocalClass(method.declaringType)) false
    else if (scalaVersion.isScala2 && isNestedClass(method.declaringType)) false
    else if (isDefaultValue(method)) false
    else if (isTraitInitializer(method)) skipTraitInitializer(method)
    else
      try skipScala(method)
      catch {
        case NonFatal(e) => throwOrWarn(e); false
      }
  }

  def format(method: Method): Option[String] = {
    if (method.isBridge) None
    else if (isDynamicClass(method.declaringType)) None
    else if (isJava(method)) Some(formatJava(method))
    // TODO add in shouldSkipOver and test in StepFilterBridgeTest
    else if (isStaticMain(method)) None
    else if (isStaticConstructor(method)) Some(formatJava(method))
    else if (isAdaptedMethod(method)) None
    else if (isAnonFunction(method)) Some(formatJava(method))
    else if (isAnonClass(method.declaringType)) Some(formatJava(method))
    // TODO in Scala 3 we should be able to find the symbol of a local class using TASTy Query
    else if (isLocalClass(method.declaringType)) Some(formatJava(method))
    else if (scalaVersion.isScala2 && isNestedClass(method.declaringType)) Some(formatJava(method))
    else
      try formatScala(method)
      catch {
        case NonFatal(e) => throwOrWarn(e); Some(formatJava(method))
      }
  }

  private def formatJava(method: Method): String = {
    val declaringType = method.declaringType().name.split("\\.").last
    val methodName = method.name()
    val argumentTypes = method.argumentTypes.asScala.toList
      .map(t => t.name().split("\\.").last)
      .mkString(",")
    val returnType = method.returnTypeName().split("\\.").last
    s"$declaringType.$methodName(${if (argumentTypes.nonEmpty) argumentTypes else ""}): $returnType"
  }

  private def isStaticMain(m: Method): Boolean =
    m.isStatic && m.name == "main"

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

  private def isStaticConstructor(method: Method): Boolean =
    method.name == "<clinit>"

  private def isAnonFunction(method: Method): Boolean =
    method.name.matches(".+\\$anonfun\\$\\d+")

  private def isLiftedMethod(method: Method): Boolean =
    method.name.matches(".+\\$\\d+")

  private def isAdaptedMethod(method: Method): Boolean =
    method.name.matches(".+\\$adapted(\\$\\d+)?")

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

  private def isDefaultValue(method: Method): Boolean =
    method.name.contains("$default$")

  private def isAnonClass(tpe: ReferenceType): Boolean =
    tpe.name.contains("$anon$")

  /** is local class or local object */
  private def isLocalClass(tpe: ReferenceType): Boolean =
    tpe.name.matches(".+\\$\\d+\\$?")

  private def isNestedClass(tpe: ReferenceType): Boolean =
    tpe.name.matches(".+\\$\\.+")

  private def isTraitInitializer(method: Method): Boolean =
    method.name == "$init$"

  private def skipTraitInitializer(method: Method): Boolean =
    method.bytecodes.toSeq == Seq(ByteCodes.RETURN)
}

object ScalaUnpickler {
  def apply(
      debuggee: Debuggee,
      tools: DebugTools,
      logger: Logger,
      testMode: Boolean
  ): ScalaUnpickler = {
    if (debuggee.scalaVersion.isScala2)
      new Scala2Unpickler(tools.sourceLookUp, debuggee.scalaVersion, logger, testMode)
    else
      tools.unpickler
        .flatMap { classLoader =>
          Scala3UnpicklerBridge
            .tryLoad(debuggee, classLoader, logger, testMode)
            .warnFailure(logger, s"Cannot load step filter for Scala ${debuggee.scalaVersion}")
        }
        .getOrElse(fallback(debuggee.scalaVersion, testMode))
  }

  private def fallback(scalaVersion: ScalaVersion, testMode: Boolean): ScalaUnpickler =
    new ScalaUnpickler(scalaVersion, testMode) {
      override protected def skipScala(method: Method): Boolean = false
    }
}
