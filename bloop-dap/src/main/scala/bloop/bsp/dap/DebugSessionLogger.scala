package bloop.bsp.dap

import java.net.InetSocketAddress
import dap.DebugSessionCallbacks
import bloop.logging.Logger
import bloop.logging.DebugFilter
import monix.execution.atomic.Atomic

class DebugSessionLogger(callbacks: DebugSessionCallbacks, underlying: Logger) extends Logger {
  private val initialized = Atomic(false)
  
  private final val JDINotificationPrefix = "Listening for transport dt_socket at address: "
  override val name: String = s"${underlying.name}-debug"

  override def isVerbose: Boolean = underlying.isVerbose
  override def trace(t: Throwable): Unit = underlying.trace(t)
  override def printDebug(msg: String): Unit = underlying.debug(msg)(DebugFilter.All)
  override def warn(msg: String): Unit = underlying.warn(msg)
  override def debug(msg: String)(implicit ctx: DebugFilter): Unit = underlying.debug(msg)(ctx)
  override def ansiCodesSupported(): Boolean = underlying.ansiCodesSupported()

  override def debugFilter: DebugFilter = underlying.debugFilter
  override def asVerbose: Logger =
    new DebugSessionLogger(callbacks, underlying.asVerbose)
  override def asDiscrete: Logger =
    new DebugSessionLogger(callbacks, underlying.asDiscrete)
  override def withOriginId(originId: Option[String]): Logger =
    new DebugSessionLogger(callbacks,underlying.withOriginId(originId))

  override def error(msg: String): Unit = {
    underlying.error(msg)
    callbacks.logError(msg)
  }

  override def info(msg: String): Unit = {
    underlying.info(msg)
    // Expect the first log to be JDI notification since debuggee runs with `quiet=n` JDI option
    if (msg.startsWith(JDINotificationPrefix)) {
      if (initialized.compareAndSet(false, true)) {
        val port = Integer.parseInt(msg.drop(JDINotificationPrefix.length))
        val address = new InetSocketAddress("127.0.0.1", port)
        callbacks.onListening(address)
      }
    } else {
      callbacks.logInfo(msg)
    }
  }
}