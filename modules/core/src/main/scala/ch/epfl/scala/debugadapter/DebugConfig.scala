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
    def canUseCompiler: Boolean = true
    def canBypassCompiler: Boolean = true
  }

  case object ExpressionCompilerOnly extends EvaluationMode {
    override def canBypassCompiler: Boolean = false
  }
  case object AlwaysBypassCompiler extends EvaluationMode {
    override def canUseCompiler: Boolean = false
  }
  case object MixedEvaluation extends EvaluationMode
}
