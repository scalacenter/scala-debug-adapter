package dap

import java.net.InetSocketAddress

trait DebugSessionCallbacks {
  def onListening(address: InetSocketAddress): Unit
  def printlnOut(msg: String): Unit
  def printlnErr(msg: String): Unit
}
