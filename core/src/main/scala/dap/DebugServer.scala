package dap

import java.net.{ServerSocket, URI}
import com.microsoft.java.debug.core.DebugSettings
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.atomic.AtomicBoolean

import scala.concurrent.Promise
import scala.concurrent.ExecutionContext
import java.util.concurrent.ConcurrentLinkedQueue

final class StartedDebugServer(
    val address: Task[Option[URI]],
    val listen: Task[Unit]
)

object DebugServer {

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

  def start(
      runner: DebuggeeRunner,
      logger: Logger,
      ioScheduler: Scheduler
  )(implicit executionContext: ExecutionContext): StartedDebugServer = {
    /*
     * Set backlog to 1 to recommend the OS to process one connection at a time,
     * which can happen when a restart is request and the client immediately
     * connects without waiting for the other session to finish.
     */
    val handle = ServerHandle.Tcp(backlog = 1)

    val closedServer = AtomicBoolean(false)
    val listeningPromise = Promise[Option[URI]]()
    val ongoingSessions = new ConcurrentLinkedQueue[Cancelable]()

    def listen(serverSocket: ServerSocket): Task[Unit] = {
      val session = Task {
        listeningPromise.trySuccess(Some(handle.uri))
        val socket = serverSocket.accept()
        val session = DebugSession(socket, runner, logger, ioScheduler)
        ongoingSessions.add(session)

        session.startDebuggeeAndServer()
        session.exitStatus
      }.flatten

      session
        .restartUntil(_ == DebugSession.Terminated)
        .map(_ => ())
    }

    def closeServer(t: Option[Throwable]): Task[Unit] = {
      Task {
        if (closedServer.compareAndSet(false, true)) {
          listeningPromise.trySuccess(None)
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

    val uri = Task.fromFuture(listeningPromise.future)
    val startAndListen = listen(handle.server)
      .doOnFinish(closeServer)
      .doOnCancel(closeServer(None))

    new StartedDebugServer(uri, startAndListen)
  }
}
