package ch.epfl.scala.debugadapter

object NoopLogger extends Logger {
  override def debug(msg: => String): Unit = ()
  override def info(msg: => String): Unit = ()
  override def warn(msg: => String): Unit = ()
  override def error(msg: => String): Unit = System.err.println(msg)
  override def trace(t: => Throwable): Unit = ()
}

/**
  * Can be used in DebugServer for debugging
  */
object PrintLogger extends Logger {
  override def debug(msg: => String): Unit = println(s"[DEBUG] $msg")
  override def info(msg: => String): Unit = println(s"[INFO] $msg")
  override def warn(msg: => String): Unit = println(s"[WARN] $msg")
  override def error(msg: => String): Unit = System.err.println(s"[ERROR] $msg")
  override def trace(t: => Throwable): Unit = t.getStackTrace.foreach(println)
}
