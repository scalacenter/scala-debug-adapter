package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.internal.DebugSession

import java.net.{InetSocketAddress, ServerSocket, URI}
import java.util.concurrent.{ConcurrentLinkedQueue, TimeUnit}
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

final class DebugServer private (
    runner: DebuggeeRunner,
    address: DebugServer.Address,
    logger: Logger,
    autoCloseSession: Boolean,
    gracePeriod: Duration,
    testMode: Boolean
)(implicit ec: ExecutionContext) {
  private var closedServer = false
  private val ongoingSessions = new ConcurrentLinkedQueue[DebugSession]()
  private val lock = new Object()

  private val serverSocket = address.serverSocket
  val uri = address.uri

  /**
   * Wait for a connection then start a session
   * If the session returns `DebugSession.Restarted`, wait for a new connection and start a new session
   * Until the session returns `DebugSession.Terminated` or `DebugSession.Disconnected`
   */
  def start(): Future[Unit] = {
    for {
      session <- Future(connect())
      exitStatus <- session.exitStatus
      _ <- exitStatus match {
        case DebugSession.Restarted => start()
        case _ => Future.successful(())
      }
    } yield ()
  }

  /**
   * Connect once and return a running session
   * In case of race condition with the [[close]] method, the session can be closed before returned
   */
  private[debugadapter] def connect(): DebugSession = {
    val socket = serverSocket.accept()
    val session =
      DebugSession(
        socket,
        runner,
        logger,
        autoCloseSession,
        gracePeriod,
        testMode
      )
    lock.synchronized {
      if (closedServer) {
        session.close()
      } else {
        ongoingSessions.add(session)
        session.start()
      }
      session
    }
  }

  def close(): Unit = {
    lock.synchronized {
      if (!closedServer) {
        closedServer = true
        ongoingSessions.forEach(_.close())
        try {
          logger.info(s"Closing debug server $uri")
          serverSocket.close()
        } catch {
          case NonFatal(e) =>
            logger.warn(
              s"Could not close debug server listening on $uri due to: ${e.getMessage}]"
            )
        }
      }
    }
  }
}

object DebugServer {

  final class Address() {
    private val address = new InetSocketAddress(0)
    /*
     * Set backlog to 1 to recommend the OS to process one connection at a time,
     * which can happen when a restart is request and the client immediately
     * connects without waiting for the other session to finish.
     */
    val serverSocket =
      new ServerSocket(address.getPort, 1, address.getAddress)

    def uri: URI =
      URI.create(s"tcp://${address.getHostString}:${serverSocket.getLocalPort}")
  }

  final class Handler(val uri: URI, val running: Future[Unit])

  /**
   * Create the server.
   * The server must then be started manually
   *
   * @param runner The debuggee process
   * @param address
   * @param logger
   * @param autoCloseSession If true the session closes itself after receiving terminated event from the debuggee
   * @param gracePeriod When closed the session waits for the debuggee to terminated gracefully
   * @param ec
   * @return a new instance of DebugServer
   */
  def apply(
      runner: DebuggeeRunner,
      address: DebugServer.Address,
      logger: Logger,
      autoCloseSession: Boolean = false,
      gracePeriod: Duration = Duration(5, TimeUnit.SECONDS),
      testMode: Boolean = false
  )(implicit ec: ExecutionContext): DebugServer = {
    new DebugServer(
      runner,
      address,
      logger,
      autoCloseSession,
      gracePeriod,
      testMode
    )
  }

  /**
   * Create a new server and start it.
   * The server waits for a connection then starts a session
   * If the session returns Restarted, the server will wait for a new connection
   * If the session returns Terminated or Disconnected it stops
   *
   * @param runner The debuggee process
   * @param logger
   * @param autoCloseSession If true the session closes itself after receiving terminated event from the debuggee
   * @param gracePeriod When closed the session waits for the debuggee to terminated gracefully
   * @param ec
   * @return The uri and running future of the server
   */
  def start(
      runner: DebuggeeRunner,
      logger: Logger,
      autoCloseSession: Boolean = false,
      gracePeriod: Duration = Duration(2, TimeUnit.SECONDS)
  )(implicit ec: ExecutionContext): Handler = {
    val address = new DebugServer.Address()
    val server =
      new DebugServer(
        runner,
        address,
        logger,
        autoCloseSession,
        gracePeriod,
        false
      )
    val running = server.start()
    running.onComplete(_ => server.close())
    new Handler(server.uri, running)
  }
}
