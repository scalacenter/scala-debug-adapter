package ch.epfl.scala.debugadapter.internal;

import ch.epfl.scala.debugadapter.StepFiltersConfig

case class ScalaLaunchArguments(
    noDebug: Boolean,
    scalaStepFilters: StepFiltersConfig
)
