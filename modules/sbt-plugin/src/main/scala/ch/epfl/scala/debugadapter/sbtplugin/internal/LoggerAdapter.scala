package ch.epfl.scala.debugadapter.sbtplugin.internal

import ch.epfl.scala.debugadapter.Logger

/**
 * Underlying logger is mutable because we use a different logger
 * for logging during the task and during the background job
 */
private[debugadapter] class LoggerAdapter(var underlying: sbt.Logger) extends Logger {
  override def debug(msg: => String): Unit = ()
  override def info(msg: => String): Unit = underlying.info(msg)
  override def warn(msg: => String): Unit = underlying.warn(msg)
  override def error(msg: => String): Unit = underlying.error(msg)
  override def trace(t: => Throwable): Unit = underlying.trace(t)
}
