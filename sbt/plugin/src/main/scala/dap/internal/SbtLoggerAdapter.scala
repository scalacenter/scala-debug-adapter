package dap.internal

import dap.Logger

private[dap] class LoggerAdapter(underlying: sbt.Logger) extends Logger {
  override def debug(msg: => String): Unit = underlying.debug(msg)
  override def info(msg: => String): Unit = underlying.info(msg)
  override def warn(msg: => String): Unit = underlying.warn(msg)
  override def error(msg: => String): Unit = underlying.error(msg)
  override def trace(t: => Throwable): Unit = underlying.trace(t)
}
