package ch.epfl.scala.debugadapter.internal

import com.microsoft.java.debug.core.DebugException
import com.microsoft.java.debug.core.adapter.ErrorCode

object Errors {
  def hotCodeReplaceFailure(message: String): DebugException =
    userError(s"hot code replace failed: $message", ErrorCode.HCR_FAILURE)

  def runtimeValidationFailure(message: String): DebugException =
    userError(s"runtime validation failed: $message", ErrorCode.EVALUATE_FAILURE)

  def frameDecodingFailure(message: String): DebugException =
    error(s"decoding frame failed: $message", ErrorCode.STEP_FAILURE)

  def compilationFailure(errors: Seq[String]): DebugException =
    userError("compilation failed:\n" + errors.mkString("\n"), ErrorCode.EVALUATION_COMPILE_ERROR)

  def evaluationFailure(message: String): DebugException =
    userError(s"evaluation failed: $message", ErrorCode.EVALUATE_FAILURE)

  def frameDecodingFailure(cause: Throwable): DebugException =
    error(cause, ErrorCode.STEP_FAILURE)

  private def userError(message: String, code: ErrorCode): DebugException =
    new DebugException(message, code.getId, true)

  private def error(message: String, code: ErrorCode): DebugException =
    new DebugException(message, code.getId)

  private def error(cause: Throwable, code: ErrorCode): DebugException =
    new DebugException(cause, code.getId)
}
