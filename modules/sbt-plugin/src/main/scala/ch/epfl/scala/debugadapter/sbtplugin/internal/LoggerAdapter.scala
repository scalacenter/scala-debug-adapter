package ch.epfl.scala.debugadapter.sbtplugin.internal

import ch.epfl.scala.debugadapter.Logger

private[debugadapter] class LoggerAdapter(underlying: sbt.Logger) extends Logger {
  override def debug(msg: => String): Unit = ()
  override def info(msg: => String): Unit = underlying.info(msg)
  override def warn(msg: => String): Unit = underlying.warn(msg)
  override def error(msg: => String): Unit = underlying.error(msg)
  override def trace(t: => Throwable): Unit = underlying.trace(t)
}
