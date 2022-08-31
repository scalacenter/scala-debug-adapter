package ch.epfl.scala.debugadapter.internal.stepfilter

import com.sun.jdi
import ch.epfl.scala.debugadapter.internal.SourceLookUpProvider
import ch.epfl.scala.debugadapter.DebuggeeRunner
import ch.epfl.scala.debugadapter.Logger

trait ScalaVersionStepFilter {
  def skipMethod(method: jdi.Method): Boolean
}

object ScalaVersionStepFilter {
  def apply(
      sourceLookUp: SourceLookUpProvider,
      runner: DebuggeeRunner,
      logger: Logger,
      testMode: Boolean
  ): ScalaVersionStepFilter = {
    if (runner.scalaVersion.startsWith("2"))
      new Scala2StepFilter(sourceLookUp, runner.scalaVersion, logger, testMode)
    else
      Scala3StepFilter.tryLoad(runner, logger, testMode).getOrElse(fallback)
  }

  private def fallback: ScalaVersionStepFilter = new ScalaVersionStepFilter {
    def skipMethod(method: jdi.Method): Boolean = false
  }
}
