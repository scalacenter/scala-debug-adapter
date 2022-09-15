package ch.epfl.scala.debugadapter.internal.stepfilter

import com.sun.jdi.Method
import com.sun.jdi.ReferenceType
import com.sun.jdi.AbsentInformationException
import ch.epfl.scala.debugadapter.internal.ByteCodes
import ch.epfl.scala.debugadapter.internal.SourceLookUpProvider
import ch.epfl.scala.debugadapter.DebuggeeRunner
import ch.epfl.scala.debugadapter.Logger

trait ScalaStepFilter extends StepFilter {
  protected def skipScalaMethod(method: Method): Boolean

  override def skip(method: Method): Boolean = {
    if (method.isBridge) true
    else if (isDynamicClass(method.declaringType)) true
    else if (isJava(method)) false
    else if (isLocalMethod(method)) false
    else if (isAnonFunction(method)) false
    else if (isLocalClass(method.declaringType)) false
    else if (isDefaultValue(method)) false
    else if (isTraitInitializer(method)) skipTraitInitializer(method)
    else skipScalaMethod(method)
  }

  private def isDynamicClass(tpe: ReferenceType): Boolean =
    try {
      tpe.sourceName()
      false
    } catch {
      case _: AbsentInformationException =>
        // We assume that a ReferenceType with no source name is necessarily a dynamic class
        true
    }

  private def isJava(method: Method): Boolean =
    method.declaringType.sourceName.endsWith(".java")

  private def isLocalMethod(method: Method): Boolean =
    method.name.matches(".+\\$\\d+")

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
      sourceLookUp: SourceLookUpProvider,
      runner: DebuggeeRunner,
      logger: Logger,
      testMode: Boolean
  ): StepFilter = {
    if (runner.scalaVersion.startsWith("2"))
      new Scala2StepFilter(sourceLookUp, runner.scalaVersion, logger, testMode)
    else
      Scala3StepFilter
        .tryLoad(runner, logger, testMode)
        .getOrElse {
          logger.warn(
            s"Cannot load step filter for Scala ${runner.scalaVersion}"
          )
          fallback
        }
  }

  private def fallback: StepFilter = new ScalaStepFilter {
    override protected def skipScalaMethod(method: Method): Boolean = false
  }
}
