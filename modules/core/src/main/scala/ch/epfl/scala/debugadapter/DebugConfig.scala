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

  sealed trait EvaluationMode {
    def allowScalaEvaluation: Boolean = false
    def allowSimpleEvaluation: Boolean = false
    def allowRuntimeEvaluation: Boolean = false
  }

  case object ScalaEvaluationOnly extends EvaluationMode {
    override def allowScalaEvaluation: Boolean = true
  }
  case object SimpleEvaluationOnly extends EvaluationMode {
    override def allowSimpleEvaluation: Boolean = true
  }

  case object RuntimeEvaluationOnly extends EvaluationMode {
    override def allowRuntimeEvaluation: Boolean = true
  }

  case object NoEvaluation extends EvaluationMode
  case object MixedEvaluation extends EvaluationMode {
    override def allowScalaEvaluation: Boolean = true
    override def allowSimpleEvaluation: Boolean = true
    override def allowRuntimeEvaluation: Boolean = false
  }
}
