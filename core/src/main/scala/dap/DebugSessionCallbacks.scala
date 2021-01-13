package dap

import java.net.InetSocketAddress
import scala.util.Try
import com.microsoft.java.debug.core.protocol.Events
import scala.concurrent.Future

trait DebugSessionCallbacks {
  def onListening(address: InetSocketAddress): Unit
  def onFinish(status: Try[ExitStatus]): Future[Unit]
  def onCancel(): Future[Unit]
  def logError(msg: String): Unit
  def logInfo(msg: String): Unit
}