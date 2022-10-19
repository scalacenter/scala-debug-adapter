package ch.epfl.scala.debugadapter.internal

import com.microsoft.java.debug.core.adapter.{StepFilterProvider => JavaStepFilterProvider}
import com.microsoft.java.debug.core.protocol.Requests.StepFilters
import com.sun.jdi.Method

import ch.epfl.scala.debugadapter.Logger
import ch.epfl.scala.debugadapter.internal.stepfilter._
import ch.epfl.scala.debugadapter.Debuggee
import ch.epfl.scala.debugadapter.DebugTools
import com.sun.jdi.Location

class StepFilterProvider(
    stepFilters: Seq[StepFilter],
    logger: Logger,
    testMode: Boolean
) extends JavaStepFilterProvider() {

  override def shouldSkipOver(method: Method, filters: StepFilters): Boolean = {
    try {
      val shouldSkip = super.shouldSkipOver(method, filters) || stepFilters.exists(_.shouldSkipOver(method))
      if (shouldSkip) logger.debug(s"Skipping $method (step into)")
      shouldSkip
    } catch {
      case cause: Throwable =>
        if (testMode) throw cause
        else logger.error(s"Failed to determine if $method should be stepped into: ${cause.getMessage}")
        false
    }
  }

  override def shouldSkipOut(upperLocation: Location, method: Method): Boolean = {
    try {
      val shouldSkip =
        super.shouldSkipOut(upperLocation, method) ||
          stepFilters.exists(_.shouldSkipOut(upperLocation, method))
      if (shouldSkip) logger.debug(s"Skipping $method (step out)")
      shouldSkip
    } catch {
      case cause: Throwable =>
        if (testMode) throw cause
        else logger.error(s"Failed to determine if $method should be stepped out: ${cause.getMessage}")
        false
    }
  }
}

object StepFilterProvider {
  def apply(
      debuggee: Debuggee,
      tools: DebugTools,
      sourceLookUp: SourceLookUpProvider,
      logger: Logger,
      testMode: Boolean
  ): StepFilterProvider = {
    val scalaStepFilter = ScalaStepFilter(debuggee, tools, sourceLookUp, logger, testMode)
    val runtimeStepFilter = RuntimeStepFilter(debuggee.scalaVersion)
    new StepFilterProvider(
      Seq(ClassLoadingStepFilter, runtimeStepFilter, scalaStepFilter),
      logger,
      testMode
    )
  }
}
