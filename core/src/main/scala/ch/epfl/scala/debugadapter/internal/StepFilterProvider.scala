package ch.epfl.scala.debugadapter.internal

import com.microsoft.java.debug.core.adapter.{
  StepFilterProvider => JavaStepFilterProvider
}
import com.microsoft.java.debug.core.protocol.Requests.StepFilters
import com.sun.jdi
import com.sun.jdi.AbsentInformationException

import ch.epfl.scala.debugadapter.Logger
import ch.epfl.scala.debugadapter.internal.stepfilter.ScalaVersionStepFilter
import ch.epfl.scala.debugadapter.DebuggeeRunner

class StepFilterProvider(
    scalaStepFilter: ScalaVersionStepFilter,
    logger: Logger,
    testMode: Boolean
) extends JavaStepFilterProvider() {

  override def skip(method: jdi.Method, filters: StepFilters): Boolean = {
    try {
      val res =
        if (method.isBridge) true
        else if (isDynamicClass(method.declaringType)) true
        else if (super.skip(method, filters)) true
        else if (isJava(method)) false
        else if (isLocalMethod(method)) false
        else if (isLocalClass(method.declaringType)) false
        else if (isDefaultValue(method)) false
        else if (isTraitInitializer(method)) skipTraitInitializer(method)
        else scalaStepFilter.skipMethod(method)

      if (res) logger.debug(s"Skipping $method")
      res
    } catch {
      case e: Exception =>
        if (testMode) throw e
        else {
          logger.error(
            s"Failed to determine if ${method} should be skipped: ${e.getMessage}"
          )
          logger.trace(e)
        }
        false
    }
  }

  private def isDynamicClass(tpe: jdi.ReferenceType): Boolean =
    try {
      tpe.sourceName()
      false
    } catch {
      case _: AbsentInformationException =>
        // We assume that a ReferenceType with no source name is necessarily a dynamic class
        true
    }

  private def isJava(method: jdi.Method): Boolean =
    method.declaringType.sourceName.endsWith(".java")

  private def isLocalMethod(method: jdi.Method): Boolean =
    method.name.contains("$anonfun$")

  private def isDefaultValue(method: jdi.Method): Boolean =
    method.name.contains("$default$")

  private def isLocalClass(tpe: jdi.ReferenceType): Boolean =
    tpe.name.contains("$anon$")

  private def isTraitInitializer(method: jdi.Method): Boolean =
    method.name == "$init$"

  private def skipTraitInitializer(method: jdi.Method): Boolean =
    method.bytecodes.toSeq == Seq(ByteCodes.RETURN)
}

object StepFilterProvider {
  def apply(
      sourceLookUp: SourceLookUpProvider,
      runner: DebuggeeRunner,
      logger: Logger,
      testMode: Boolean
  ): StepFilterProvider = {
    val scalaStepFilter =
      ScalaVersionStepFilter(sourceLookUp, runner, logger, testMode)
    new StepFilterProvider(scalaStepFilter, logger, testMode)
  }
}
