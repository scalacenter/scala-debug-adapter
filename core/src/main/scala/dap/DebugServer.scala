package dap

import com.microsoft.java.debug.core.DebugSettings
import dap.DebugSession.{Restarted, Terminated}

import java.net.{InetSocketAddress, ServerSocket, URI}
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._
import scala.util.control.NonFatal

class DebugServer(runner: DebuggeeRunner, logger: Logger)(implicit ec: ExecutionContext) {

  /**
   * Disable evaluation of variable's `toString` methods
   * since code evaluation is not supported.
   *
   * Debug adapter, when asked for variables, tries to present them in a readable way,
   * hence it evaluates the `toString` method for each object providing it.
   * The adapter is not checking if evaluation is supported, so the whole request
   * fails if there is at least one variable with custom `toString` in scope.
   *
   * See usages of [[com.microsoft.java.debug.core.adapter.variables.VariableDetailUtils.formatDetailsValue()]]
   */
  DebugSettings.getCurrent.showToString = false

  private val address = new InetSocketAddress(0)
  private val closedServer = new AtomicBoolean(false)
  private val ongoingSessions = new ConcurrentLinkedQueue[DebugSession]()

  /*
   * Set backlog to 1 to recommend the OS to process one connection at a time,
   * which can happen when a restart is request and the client immediately
   * connects without waiting for the other session to finish.
   */
  private val server = new ServerSocket(address.getPort, 1, address.getAddress)

  val uri: URI = URI.create(s"tcp://${address.getHostString}:${server.getLocalPort}")

  /**
    * Continually connect until current session returns `Terminated`
    */
  def start(): Future[Unit] = Future {
    val session = connect()
    session.exitStatus.map {
      case Restarted => start()
      case Terminated => Future.successful(())
    }.flatten
  }

  /**
    * Connect once and return current session
    */
  def connect(): DebugSession = {
    val socket = server.accept()
    val session = DebugSession(socket, runner, logger)
    ongoingSessions.add(session)
    session.start()
    session
  }

  def close(gracePeriod: Duration = 1 millisecond): Unit = {
    if (closedServer.compareAndSet(false, true)) {
      ongoingSessions.forEach(_.cancel(gracePeriod))
      try server.close()
      catch {
        case NonFatal(e) =>
          logger.warn(
            s"Could not close debug server listening on [$uri due to: ${e.getMessage}]"
          )
      }
    }
  }
}
