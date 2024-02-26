package ch.epfl.scala.debugadapter.internal;

class PartialLaunchArguments {
  val noDebug = false
  val scalaStepFilters = PartialLaunchArguments.UsedStepFilters()

}

object PartialLaunchArguments {
  case class UsedStepFilters(decoder: Boolean = true, classLoading: Boolean = true, runtime: Boolean = true)
}
