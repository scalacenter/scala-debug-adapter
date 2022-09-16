package ch.epfl.scala.debugadapter.internal

import com.microsoft.java.debug.core.adapter.{
  StepFilterProvider => JavaStepFilterProvider
}
import com.microsoft.java.debug.core.protocol.Requests.StepFilters
import com.sun.jdi.Method

import ch.epfl.scala.debugadapter.Logger
import ch.epfl.scala.debugadapter.internal.stepfilter._
import ch.epfl.scala.debugadapter.DebuggeeRunner

class StepFilterProvider(
    stepFilters: Seq[StepFilter],
    logger: Logger,
    testMode: Boolean
) extends JavaStepFilterProvider() {

  override def skip(method: Method, filters: StepFilters): Boolean = {
    try {
      val shouldSkip =
        super.skip(method, filters) || stepFilters.exists(_.skip(method))
      if (shouldSkip) logger.debug(s"Skipping $method")
      shouldSkip
    } catch {
      case e: Exception =>
        if (testMode) throw e
        else {
          val msg =
            s"Failed to determine if $method should be skipped: ${e.getMessage}"
          logger.error(msg)
        }
        false
    }
  }
}

object StepFilterProvider {
  def apply(
      sourceLookUp: SourceLookUpProvider,
      runner: DebuggeeRunner,
      logger: Logger,
      testMode: Boolean
  ): StepFilterProvider = {
    val scalaStepFilter =
      ScalaStepFilter(sourceLookUp, runner, logger, testMode)
    val runtimeStepFilter = RuntimeStepFilter(runner)
    new StepFilterProvider(
      Seq(runtimeStepFilter, scalaStepFilter),
      logger,
      testMode
    )
  }
}
