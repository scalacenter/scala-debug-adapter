package dap

import sbt.internal.util.ManagedLogger


class LoggerAdapter(underlying: sbt.Logger) extends Logger {
  override def debug(msg: => String): Unit = underlying.debug(msg)
  override def info(msg: => String): Unit = underlying.info(msg)
  override def warn(msg: => String): Unit = underlying.warn(msg)
  override def error(msg: => String): Unit = underlying.error(msg)
  override def trace(t: => Throwable): Unit = underlying.trace(t)
}
