package ch.epfl.scala.debugadapter.internal;

import com.microsoft.java.debug.core.protocol.Requests.LaunchArguments
import ch.epfl.scala.debugadapter.StepFiltersConfig

case class PartialLaunchArguments(
    scalaStepFilters: StepFiltersConfig
) extends LaunchArguments
