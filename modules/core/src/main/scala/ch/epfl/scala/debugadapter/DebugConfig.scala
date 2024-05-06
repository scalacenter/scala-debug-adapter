package ch.epfl.scala.debugadapter

import scala.concurrent.duration.*

case class StepFiltersConfig(
    skipForwardersAndAccessors: Boolean,
    skipRuntimeClasses: Boolean,
    skipClassLoading: Boolean
)

object StepFiltersConfig {
  val default = StepFiltersConfig(skipForwardersAndAccessors = true, skipRuntimeClasses = true, skipClassLoading = true)
}

final case class DebugConfig(
    gracePeriod: Duration,
    testMode: Boolean,
    evaluationMode: DebugConfig.EvaluationMode,
    stepFilters: StepFiltersConfig
)

object DebugConfig {
  def default: DebugConfig = DebugConfig(
    5.seconds,
    testMode = false,
    evaluationMode = DebugConfig.MixedEvaluation,
    stepFilters = StepFiltersConfig.default
  )

  sealed trait EvaluationMode {
    def allowScalaEvaluation: Boolean = false
    def allowRuntimeEvaluation: Boolean = false
  }

  case object ScalaEvaluationOnly extends EvaluationMode {
    override def allowScalaEvaluation: Boolean = true
  }
  case object RuntimeEvaluationOnly extends EvaluationMode {
    override def allowRuntimeEvaluation: Boolean = true
  }

  case object NoEvaluation extends EvaluationMode
  case object MixedEvaluation extends EvaluationMode {
    override def allowScalaEvaluation: Boolean = true
    override def allowRuntimeEvaluation: Boolean = true
  }
}
