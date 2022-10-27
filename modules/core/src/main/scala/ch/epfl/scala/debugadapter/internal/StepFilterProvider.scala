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
      val skipOver = super.shouldSkipOver(method, filters) || stepFilters.exists(_.shouldSkipOver(method))
      if (skipOver) logger.debug(s"Skipping over $method")
      skipOver
    } catch {
      case cause: Throwable =>
        if (testMode) throw cause
        logger.warn(s"Failed to determine if $method should be skipped over: ${cause.getMessage}")
        false
    }
  }

  override def shouldSkipOut(upperLocation: Location, method: Method): Boolean = {
    try {
      val skipOut =
        super.shouldSkipOut(upperLocation, method) ||
          stepFilters.exists(_.shouldSkipOut(upperLocation, method))
      if (skipOut) logger.debug(s"Skipping out $method")
      skipOut
    } catch {
      case cause: Throwable =>
        if (testMode) throw cause
        logger.warn(s"Failed to determine if $method should be skipped out: ${cause.getMessage}")
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
