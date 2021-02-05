package dap

import java.net.InetSocketAddress

trait DebuggeeLogger {
  def onListening(address: InetSocketAddress): Unit
  def out(line: String): Unit
  def err(line: String): Unit
}
