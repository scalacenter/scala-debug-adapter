package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.Debuggee
import ch.epfl.scala.debugadapter.Logger
import ch.epfl.scala.debugadapter.internal.stacktrace.*
import com.microsoft.java.debug.core.adapter.{StackTraceProvider => JavaStackTraceProvider}
import com.microsoft.java.debug.core.protocol.Requests.StepFilters
import com.sun.jdi.Location
import com.sun.jdi.Method
import com.microsoft.java.debug.core.adapter.stacktrace.DecodedMethod
import ch.epfl.scala.debugadapter.DebugConfig

class StackTraceProvider(
    stepFilters: Seq[StepFilter],
    protected val logger: Logger,
    protected val testMode: Boolean
) extends JavaStackTraceProvider
    with ThrowOrWarn {
  val decoder = stepFilters.collectFirst { case u: ScalaDecoder => u }
  def reload(): Unit = decoder.foreach(_.reload())

  override def decode(method: Method): DecodedMethod =
    decoder.map(_.decode(method)).getOrElse(JavaMethod(method, isGenerated = false))

  override def skipOver(method: Method, filters: StepFilters): Boolean = {
    try {
      val skipOver = super.skipOver(method, filters) || stepFilters.exists(_.skipOver(method))
      if (skipOver) logger.debug(s"Skipping over $method")
      skipOver
    } catch {
      case cause: Throwable =>
        throwOrWarn(s"Failed to determine if $method should be skipped over: ${cause.getMessage}")
        false
    }
  }

  override def skipOut(upperLocation: Location, method: Method): Boolean = {
    try {
      val skipOut =
        super.skipOut(upperLocation, method) ||
          stepFilters.exists(_.skipOut(upperLocation, method))
      if (skipOut) logger.debug(s"Skipping out $method")
      skipOut
    } catch {
      case cause: Throwable =>
        throwOrWarn(s"Failed to determine if $method should be skipped out: ${cause.getMessage}")
        false
    }
  }
}

object StackTraceProvider {
  private def stepFiltersProvider(
      debuggee: Debuggee,
      tools: DebugTools,
      logger: Logger,
      config: DebugConfig
  ): Seq[StepFilter] = {
    var list = List.empty[StepFilter]
    if (config.stepFilters.skipClassLoading) list = ClassLoadingFilter +: list
    if (config.stepFilters.skipRuntimeClasses) list = RuntimeStepFilter(debuggee.scalaVersion) +: list
    if (config.stepFilters.skipForwardersAndAccessors)
      list = ScalaDecoder(debuggee, tools, logger, config.testMode) +: list
    list
  }

  def apply(
      debuggee: Debuggee,
      tools: DebugTools,
      logger: Logger,
      config: DebugConfig
  ): StackTraceProvider = {
    new StackTraceProvider(
      stepFiltersProvider(debuggee, tools, logger, config),
      logger,
      config.testMode
    )
  }
}
