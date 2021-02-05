package dap.internal

import scala.sys.process.ProcessLogger
import java.net.InetSocketAddress
import dap.DebuggeeLogger

private class DebuggeeProcessLogger(callbacks: DebuggeeLogger) extends ProcessLogger {
  private final val JDINotificationPrefix = "Listening for transport dt_socket at address: "

  override def out(line: => String): Unit = {
    if (line.startsWith(JDINotificationPrefix)) {
      val port = Integer.parseInt(line.drop(JDINotificationPrefix.length))
      val address = new InetSocketAddress("127.0.0.1", port)
      callbacks.onListening(address)
    } else {
      callbacks.out(line)
    }
  }
  override def err(line: => String): Unit = {
    callbacks.err(line)
  }
  override def buffer[T](f: => T): T = f
}