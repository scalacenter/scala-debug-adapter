package ch.epfl.scala.debugadapter

import scala.concurrent.duration.*

final case class DebugConfig(
    gracePeriod: Duration,
    autoCloseSession: Boolean,
    testMode: Boolean,
    evaluationMode: DebugConfig.EvaluationMode
)

object DebugConfig {
  def default: DebugConfig = DebugConfig(
    5.seconds,
    autoCloseSession = true,
    testMode = false,
    evaluationMode = DebugConfig.MixedEvaluation
  )
  sealed trait PreEvaluationMode {
    def allowPreEvaluation: Boolean
  }

  sealed trait EvaluationMode {
    def allowScalaEvaluation: Boolean = false
    def allowRuntimeEvaluation: Boolean = false
  }

  case object AllowPreEvaluation extends PreEvaluationMode {
    override def allowPreEvaluation: Boolean = true
  }
  case object NoPreEvaluation extends PreEvaluationMode {
    override def allowPreEvaluation: Boolean = false
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
