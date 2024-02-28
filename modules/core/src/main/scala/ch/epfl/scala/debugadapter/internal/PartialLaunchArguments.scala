package ch.epfl.scala.debugadapter.internal;

import com.microsoft.java.debug.core.protocol.Requests.LaunchArguments
import ch.epfl.scala.debugadapter.UsedStepFilters

case class PartialLaunchArguments(
    scalaStepFilters: UsedStepFilters
) extends LaunchArguments
