package dap

import scala.sys.process.ProcessLogger
import java.net.InetSocketAddress

class DebuggeeProcessLogger(callbacks: DebugSessionCallbacks) extends ProcessLogger {
  private final val JDINotificationPrefix = "Listening for transport dt_socket at address: "

  override def out(line: => String): Unit = {
    if (line.startsWith(JDINotificationPrefix)) {
      val port = Integer.parseInt(line.drop(JDINotificationPrefix.length))
      val address = new InetSocketAddress("127.0.0.1", port)
      callbacks.onListening(address)
    } else {
      callbacks.printlnOut(line)
    }
  }
  override def err(line: => String): Unit = {
    callbacks.printlnErr(line)
  }
  override def buffer[T](f: => T): T = f
}