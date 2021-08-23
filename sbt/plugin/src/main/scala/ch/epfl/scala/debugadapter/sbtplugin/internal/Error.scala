package ch.epfl.scala.debugadapter.sbtplugin.internal

import sbt.internal.langserver.ErrorCodes

private[debugadapter] final case class Error(code: Long, message: String)

private[debugadapter] object Error {
  def paramsMissing(method: String): Error =
    Error(ErrorCodes.InvalidParams, s"param is expected on '$method' method.")
  def invalidParams(msg: String): Error = Error(ErrorCodes.InvalidParams, msg)
  def parseError(cause: Throwable): Error =
    Error(ErrorCodes.ParseError, cause.getMessage)
}
