package sbtdap

import sbt.internal.langserver.ErrorCodes
import sbt.internal.protocol.JsonRpcRequestMessage
import sjsonnew.JsonFormat

sealed trait JsonRpcResponse

object JsonRpcResponse {
  final case class Error(code: Long, message: String) extends JsonRpcResponse
  final case class Success[A: JsonFormat](event: A, execId: Option[String]) extends JsonRpcResponse

  def paramsMissing(method: String): Error =
    Error(ErrorCodes.InvalidParams, s"param is expected on '$method' method.")

  def invalidParams(msg: String): Error =
    Error(ErrorCodes.InvalidParams, msg)

  def invalidRequest(msg: String): Error =
    Error(ErrorCodes.InvalidRequest, msg)
}