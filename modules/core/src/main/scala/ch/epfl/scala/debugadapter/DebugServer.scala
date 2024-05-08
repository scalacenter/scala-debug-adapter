package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.internal.DebugSession

import java.net.InetSocketAddress
import java.net.ServerSocket
import java.net.URI
import java.util.concurrent.atomic.AtomicReference
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
  private val onGoingSession = new AtomicReference[DebugSession]()
  private val lock = new Object()

  private val serverSocket = address.serverSocket
  val uri = address.uri

  /**
   * Wait for a connection then start a session
   * If the session returns `DebugSession.Restarted`, wait for a new connection and start a new session
   * Until the session returns `DebugSession.Terminated` or `DebugSession.Disconnected`
   */
  def run(): Future[Unit] = start().map(_ => this.close())

  private def start(): Future[Unit] = {
    for {
      session <- Future(connect())
      exitStatus <- session.exitStatus
      _ = session.close()
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
        onGoingSession.set(session)
        session.start()
      }
      session
    }
  }

  def close(): Unit = {
    lock.synchronized {
      if (!closedServer) {
        closedServer = true
        logger.info(s"Closing debug server $uri")
        try {
          onGoingSession.getAndUpdate { session =>
            if (session != null) session.close()
            null
          }
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

  def apply(
      debuggee: Debuggee,
      resolver: DebugToolsResolver,
      logger: Logger,
      address: Address = new Address,
      config: DebugConfig = DebugConfig.default
  )(implicit ec: ExecutionContext): DebugServer =
    new DebugServer(debuggee, resolver, logger, address, config)

  def run(
      debuggee: Debuggee,
      resolver: DebugToolsResolver,
      logger: Logger,
      gracePeriod: Duration = 5.seconds
  )(implicit ec: ExecutionContext): Handler = {
    val config = DebugConfig.default.copy(gracePeriod = gracePeriod)
    val server = DebugServer(debuggee, resolver, logger, config = config)
    new Handler(server.uri, server.run())
  }
}
