package ch.epfl.scala.debugadapter.sbtplugin.internal

import ch.epfl.scala.debugadapter.DebuggeeListener

import java.net.InetSocketAddress
import scala.sys.process.ProcessLogger

private class DebuggeeProcessLogger(listener: DebuggeeListener) extends ProcessLogger {
  private final val JDINotificationPrefix =
    "Listening for transport dt_socket at address: "

  override def out(line: => String): Unit = {
    if (line.startsWith(JDINotificationPrefix)) {
      val port = Integer.parseInt(line.drop(JDINotificationPrefix.length))
      val address = new InetSocketAddress("127.0.0.1", port)
      listener.onListening(address)
    } else {
      listener.out(line)
    }
  }
  override def err(line: => String): Unit = {
    listener.err(line)
  }
  override def buffer[T](f: => T): T = f
}
