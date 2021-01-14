package dap

import java.net.URI

import scala.concurrent.ExecutionContext
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.Future

class DebugServer(runner: DebuggeeRunner, logger: Logger) {
  val handle = ServerHandle.Tcp(backlog = 1)
  val closedServer = new AtomicBoolean(false)
  val ongoingSessions = new ConcurrentLinkedQueue[DebugSession]()

  def uri: Option[URI] = if (closedServer.get) None else Some(handle.uri)

  def start()(implicit ec: ExecutionContext): Future[DebugSession.ExitStatus] = {
    val socket = handle.server.accept()
    val session = DebugSession(socket, runner, logger)
    ongoingSessions.add(session)
    session.run()
    
    session.exitStatus
  }

  def close(): Unit = {
    if (closedServer.compareAndSet(false, true)) {
      ongoingSessions.forEach(_.cancel())
      try {
        handle.server.close()
      } catch {
        case e: Exception =>
          logger.warn(
            s"Could not close debug server listening on [${handle.uri} due to: ${e.getMessage}]"
          )
      }
    }
  }
}
