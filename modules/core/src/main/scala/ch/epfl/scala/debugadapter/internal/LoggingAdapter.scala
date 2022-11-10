package ch.epfl.scala.debugadapter.internal

import com.microsoft.java.debug.core.{Configuration, LoggerFactory}

import java.util.logging.{Logger => JLogger, _}

private[debugadapter] class LoggingAdapter(
    logger: ch.epfl.scala.debugadapter.Logger
) extends Handler {

  /**
   * DAP server tends to send a lot of SocketClosed exceptions when the Debuggee process has exited. This helps us filter those logs
   */
  @volatile private var closingSession = false

  override def publish(record: LogRecord): Unit = {
    val message = record.getMessage
    record.getLevel match {
      case Level.INFO | Level.CONFIG => logger.info(message)
      case Level.WARNING => logger.warn(message)
      case Level.SEVERE =>
        if (isExpectedDuringCancellation(message) || isIgnoredError(message)) logger.debug(message)
        else {
          logger.error(message)
          Option(record.getThrown).foreach(logger.trace(_))
        }
      case _ => logger.debug(message)
    }
  }

  def onClosingSession(): Unit = {
    closingSession = true
  }

  def factory: LoggerFactory = { name =>
    val logger = JLogger.getLogger(name)
    logger.getHandlers.foreach(logger.removeHandler)
    logger.setUseParentHandlers(false)
    if (name == Configuration.LOGGER_NAME) logger.addHandler(this)
    logger
  }

  override def flush(): Unit = ()

  override def close(): Unit = ()

  private final val socketClosed = "java.net.SocketException: Socket closed"

  private def isExpectedDuringCancellation(message: String): Boolean = {
    message.endsWith(socketClosed) && closingSession
  }

  private final val recordingWhenVmDisconnected =
    "Exception on recording event: com.sun.jdi.VMDisconnectedException"

  private def isIgnoredError(message: String): Boolean = {
    message.startsWith(recordingWhenVmDisconnected)
  }
}
