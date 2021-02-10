package ch.epfl.scala.debugadapter

import java.net.InetSocketAddress

trait DebuggeeListener {
  def onListening(address: InetSocketAddress): Unit
  def out(line: String): Unit
  def err(line: String): Unit
}
