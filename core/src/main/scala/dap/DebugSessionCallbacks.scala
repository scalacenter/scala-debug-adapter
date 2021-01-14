package dap

import java.net.InetSocketAddress

trait DebugSessionCallbacks {
  def onListening(address: InetSocketAddress): Unit
  def printOut(msg: String): Unit
  def printErr(msg: String): Unit
}
