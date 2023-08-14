package ch.epfl.scala.debugadapter.internal.stacktrace

trait ThrowOrWarn(warnLogger: String => Unit, testMode: Boolean):
  def throwOrWarn(message: String): Unit =
    throwOrWarn(new Exception(message))

  def throwOrWarn(exception: Throwable): Unit =
    if testMode then throw exception
    else warnLogger(exception.getMessage)
