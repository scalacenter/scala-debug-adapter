package ch.epfl.scala.debugadapter

import scala.concurrent.duration.*

case class UsedStepFilters(decoder: Boolean, runtime: Boolean, classLoading: Boolean)

object UsedStepFilters {
  val default = UsedStepFilters(decoder = true, runtime = true, classLoading = true)
}

final case class DebugConfig(
    gracePeriod: Duration,
    autoCloseSession: Boolean,
    testMode: Boolean,
    evaluationMode: DebugConfig.EvaluationMode,
    stepFilters: UsedStepFilters
)

object DebugConfig {
  def default: DebugConfig = DebugConfig(
    5.seconds,
    autoCloseSession = true,
    testMode = false,
    evaluationMode = DebugConfig.MixedEvaluation,
    stepFilters = UsedStepFilters.default
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
