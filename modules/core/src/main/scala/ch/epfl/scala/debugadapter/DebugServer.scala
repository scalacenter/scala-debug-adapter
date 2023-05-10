package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.internal.DebugSession

import java.net.InetSocketAddress
import java.net.ServerSocket
import java.net.URI
import java.util.concurrent.ConcurrentLinkedQueue
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.control.NonFatal

final class DebugServer private (
    debuggee: Debuggee,
    resolver: DebugToolsResolver,
    logger: Logger,
    address: DebugServer.Address,
    config: DebugConfig
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
    val session = DebugSession(socket, debuggee, resolver, logger, config)
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
            logger.warn(s"Could not close debug server listening on $uri due to: ${e.getMessage}]")
        }
      }
    }
  }
}

object DebugServer {
  final class Handler(val uri: URI, val running: Future[Unit])

  final class Address() {
    private val address = new InetSocketAddress(0)
    /*
     * Set backlog to 1 to recommend the OS to process one connection at a time,
     * which can happen when a restart is request and the client immediately
     * connects without waiting for the other session to finish.
     */
    val serverSocket = new ServerSocket(address.getPort, 1, address.getAddress)

    def uri: URI = URI.create(s"tcp://${address.getHostString}:${serverSocket.getLocalPort}")
  }

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
      debuggee: Debuggee,
      resolver: DebugToolsResolver,
      logger: Logger,
      address: Address = new Address,
      config: DebugConfig = DebugConfig.default
  )(implicit ec: ExecutionContext): DebugServer = {
    new DebugServer(debuggee, resolver, logger, address, config)
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
      debuggee: Debuggee,
      resolver: DebugToolsResolver,
      logger: Logger,
      autoCloseSession: Boolean = false,
      gracePeriod: Duration = 5.seconds
  )(implicit ec: ExecutionContext): Handler = {
    val config = DebugConfig.default.copy(gracePeriod = gracePeriod, autoCloseSession = autoCloseSession)
    val server = DebugServer(debuggee, resolver, logger, config = config)
    val running = server.start()
    running.onComplete(_ => server.close())
    new Handler(server.uri, running)
  }
}
