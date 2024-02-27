package ch.epfl.scala.debugadapter.internal;

import com.microsoft.java.debug.core.protocol.Requests.LaunchArguments

case class PartialLaunchArguments(
    scalaStepFilters: PartialLaunchArguments.UsedStepFilters = PartialLaunchArguments.UsedStepFilters.default
) extends LaunchArguments

object PartialLaunchArguments {
  case class UsedStepFilters(decoder: Boolean, runtime: Boolean, classLoading: Boolean)

  object UsedStepFilters {
    val default = UsedStepFilters(decoder = true, runtime = true, classLoading = true)
  }
}
