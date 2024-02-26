package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.Logger

trait ThrowOrWarn {
  protected val testMode: Boolean
  protected val logger: Logger

  protected def throwOrWarn(msg: String): Unit = {
    if (testMode) throw new Exception(msg)
    else logger.warn(msg)
  }

  protected def throwOrWarn(e: Throwable): Unit = {
    if (testMode) throw e
    else logger.warn(s"Unexpected exception ${e.getClass.getName}: ${e.getMessage}")
  }
}
