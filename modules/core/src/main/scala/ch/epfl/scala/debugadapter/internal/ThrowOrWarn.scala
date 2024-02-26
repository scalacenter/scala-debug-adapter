package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.Logger
import com.microsoft.java.debug.core.DebugException

trait ThrowOrWarn {
  protected val testMode: Boolean
  protected val logger: Logger

  protected def throwOrWarn(msg: String): Unit =
    if (testMode) throw new DebugException(msg)
    else logger.warn(msg)

  protected def throwOrWarn(e: Throwable): Unit =
    if (testMode) throw new DebugException(e)
    else logger.warn(s"Unexpected exception ${e.getClass.getName}: ${e.getMessage}")

  protected def throwOrWarn(message: String, e: Throwable) =
    if (testMode) throw new DebugException(e)
    else logger.warn(s"$message because of ${e.getClass.getSimpleName}: ${e.getMessage}")
}
