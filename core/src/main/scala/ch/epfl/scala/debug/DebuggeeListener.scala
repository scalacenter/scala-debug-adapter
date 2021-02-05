package ch.epfl.scala.debug

import java.net.InetSocketAddress

trait DebuggeeListener {
  def onListening(address: InetSocketAddress): Unit
  def out(line: String): Unit
  def err(line: String): Unit
}
