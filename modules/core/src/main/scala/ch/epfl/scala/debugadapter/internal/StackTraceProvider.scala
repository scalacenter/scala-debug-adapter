package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.Debuggee
import ch.epfl.scala.debugadapter.Logger
import ch.epfl.scala.debugadapter.internal.stacktrace.*
import com.microsoft.java.debug.core.adapter.{StackTraceProvider => JavaStackTraceProvider}
import com.microsoft.java.debug.core.protocol.Requests.StepFilters
import com.sun.jdi.Location
import com.sun.jdi.Method
import java.util.Optional

class StackTraceProvider(
    runtimeFilter: RuntimeStepFilter,
    scalaUnpickler: ScalaUnpickler,
    logger: Logger,
    testMode: Boolean
) extends JavaStackTraceProvider() {

  private val stepFilters: Seq[StepFilter] = Seq(ClassLoadingFilter, runtimeFilter, scalaUnpickler)

  override def formatMethod(method: Method): Optional[String] = {
    scalaUnpickler.format(method) match {
      case None => Optional.empty()
      case Some(s) => Optional.of(s)
    }
  }
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

object StackTraceProvider {
  def apply(
      debuggee: Debuggee,
      tools: DebugTools,
      logger: Logger,
      testMode: Boolean
  ): StackTraceProvider = {
    val scalaUnpickler: ScalaUnpickler = ScalaUnpickler(debuggee, tools, logger, testMode)
    val runtimeStepFilter = RuntimeStepFilter(debuggee.scalaVersion)
    new StackTraceProvider(
      runtimeStepFilter,
      scalaUnpickler,
      logger,
      testMode
    )
  }
}
