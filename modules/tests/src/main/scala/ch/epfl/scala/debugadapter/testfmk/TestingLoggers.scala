package ch.epfl.scala.debugadapter.testfmk

import ch.epfl.scala.debugadapter.Logger

object NoopLogger extends Logger {
  override def debug(msg: => String): Unit = ()
  override def info(msg: => String): Unit = ()
  override def warn(msg: => String): Unit = ()
  override def error(msg: => String): Unit = ()
  override def trace(t: => Throwable): Unit = ()
}

/**
 * Can be used in DebugServer for debugging
 */
object PrintLogger extends Logger {
  override def debug(msg: => String): Unit = println(s"${Console.YELLOW}[DEBUG] $msg${Console.RESET}")
  override def info(msg: => String): Unit = println(s"${Console.CYAN}[INFO] $msg${Console.RESET}")
  override def warn(msg: => String): Unit = println(s"${Console.MAGENTA}[WARN] $msg${Console.RESET}")
  override def error(msg: => String): Unit = System.err.println(s"${Console.RED}[ERROR] $msg${Console.RESET}")
  override def trace(t: => Throwable): Unit = t.printStackTrace()
}
