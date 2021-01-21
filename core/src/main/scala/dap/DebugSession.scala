package dap

import com.microsoft.java.debug.core.adapter.{IProviderContext, ProtocolServer => DapServer}
import com.microsoft.java.debug.core.protocol.Events.OutputEvent
import com.microsoft.java.debug.core.protocol.Messages.{Request, Response}
import com.microsoft.java.debug.core.protocol.Requests._
import com.microsoft.java.debug.core.protocol.{Events, JsonUtils}
import com.microsoft.java.debug.core.{Configuration, LoggerFactory}
import dap.TimeoutScheduler._

import java.net.{InetSocketAddress, Socket}
import java.util.concurrent.{CancellationException, TimeoutException}
import java.util.logging.{Handler, Level, LogRecord, Logger => JLogger}
import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

/**
 *  This debug adapter maintains the lifecycle of the debuggee in separation from JDI.
 *  The debuggee is started/closed together with the session.
 *
 *  This approach makes it necessary to handle the "launch" requests as the "attach" ones.
 *  The JDI address of the debuggee is obtained through the [[DebugSessionCallbacks]]
 * 
 *  If autoCloseSession then the session is closed automatically after the debuggee has terminated
 *  Otherwise a disconnect request should be received or the close method should be called manually
 */
final class DebugSession private (
    socket: Socket,
    runner: DebuggeeRunner,
    context: IProviderContext,
    logger: Logger,
    loggerAdapter: DebugSession.LoggerAdapter,
    autoClose: Boolean,
    gracePeriod: Duration
)(implicit executionContext: ExecutionContext) extends DapServer(
  socket.getInputStream,
  socket.getOutputStream,
  context,
  DebugSession.loggerFactory(loggerAdapter)
) {
  private type LaunchId = Int

  // A set of all processed launched requests by the client
  private val launchedRequests = mutable.Set.empty[LaunchId]

  private val terminatedEvent = Promise[Unit]()
  private val debuggeeAddress = Promise[InetSocketAddress]()
  private val attached = Promise[Unit]()
  
  private val exitStatusPromise = Promise[DebugSession.ExitStatus]()
  private val debugState: Synchronized[DebugSession.State] = new Synchronized(DebugSession.Ready)

  private[dap] def currentState: DebugSession.State = debugState.value
  private[dap] def getDebugeeAddress: Future[InetSocketAddress] = debuggeeAddress.future

  /**
   * Schedules the start of the debugging session.
   *
   * For a session to start, two executions must happen independently in a
   * non-blocking way: the debuggee process is started in the background and
   * the DAP server starts listening to client requests in an IO thread.
   */
  private[dap] def start(): Unit = {
    debugState.transform {
      case DebugSession.Ready =>
        Future(run()) // start listening
        val debuggee = runner.run(Callbacks)
        
        debuggee.future
          .onComplete { result =>
            result.failed.foreach(cancelPromises)
            // wait for the terminated event then close the session
            terminatedEvent.future.map { _ =>
              if (autoClose) {
                exitStatusPromise.trySuccess(DebugSession.Terminated)
                close()
              }
            }
          }
        
        
        DebugSession.Started(debuggee)

      case otherState =>
        otherState // don't start if already started or cancelled
    }
  }

  /**
   * Completed, once this session exit status can be determined.
   * Those are: [[DebugSession.Terminated]], [[DebugSession.Restarted]] and [[DebugSession.Disconnected]].
   * <p>Session gets the Terminated status when the communication stops before
   * the client has requested a disconnection.</p>
   * <p>Session becomes Restarted immediately when the disconnection request is received
   * and restart is set to true.</p>
   * <p>Session becomes Disconnected immediately when the disconnection request is received
   * and restart is set to false.</p>
   */
  def exitStatus: Future[DebugSession.ExitStatus]  = exitStatusPromise.future

  /**
   * Cancel the debuggee process, stop the DAP server and close the socket.
   */
  def close(): Unit = {
    super.stop()
    loggerAdapter.onClosingSession()
    debugState.transform {
      case DebugSession.Started(debuggee) =>
        cancelPromises(new CancellationException("Debug session closed"))
        debuggee.cancel()
      
        // Wait for the debuggee to terminate gracefully
        try Await.result(terminatedEvent.future, gracePeriod)
        catch {
          case _: TimeoutException =>
            logger.warn(s"Communication with debuggee $name is frozen: missing terminated event.")
        }
        DebugSession.Stopped

      case _ => DebugSession.Stopped
    }
    logger.debug(s"closing connection with debugger $name")
    socket.close()
    exitStatusPromise.trySuccess(DebugSession.Terminated)
  }

  protected override def dispatchRequest(request: Request): Unit = {
    val requestId = request.seq
    request.command match {
      case "launch" =>
        // launch request is implemented by spinning up a JVM
        // and sending an attach request to the java DapServer
        launchedRequests.add(requestId)
        debuggeeAddress
          .timeout(gracePeriod)
          .future
          .onComplete {
            case Success(address) =>
              super.dispatchRequest(DebugSession.toAttachRequest(requestId, address))
            case Failure(exception) =>
              val cause = s"Could not start debuggee $name due to: ${exception.getMessage}"
              this.sendResponse(DebugSession.failed(request, cause))
              attached.tryFailure(new IllegalStateException(cause))
              ()
          }

      case "configurationDone" =>
        // Delay handling of this request until we attach to the debuggee.
        // Otherwise, a race condition may happen when we try to communicate
        // with the VM we are not connected to
        attached.future
          .onComplete {
            case Success(_) =>
              super.dispatchRequest(request)
            case Failure(exception) =>
              sendResponse(DebugSession.failed(request, exception.getMessage))
          }

      case "disconnect" =>
        val dispatchRequest = debugState.run {
          case DebugSession.Started(debuggee) =>
            cancelPromises(new CancellationException("Client disconnected"))
            debuggee.cancel()
            (DebugSession.Stopped, true)
          case otherState => (DebugSession.Stopped, false)
        }
        
        if (dispatchRequest) {
          super.dispatchRequest(request)
        } else {
          // TODO clarify why request is not dispatched
          val ack = new Response(request.seq, request.command, true)
          sendResponse(ack)
        }

        exitStatusPromise.trySuccess {
          if (DebugSession.shouldRestart(request)) DebugSession.Restarted
          else DebugSession.Disconnected
        }
      
      case _ => super.dispatchRequest(request)
    }
  }

  protected override def sendResponse(response: Response): Unit = {
    val requestId = response.request_seq
    response.command match {
      case "attach" if launchedRequests(requestId) =>
        // attach response from java DapServer is transformed into a launch response
        // that is forwarded to the DAP client
        response.command = Command.LAUNCH.getName
        attached.success(())
        super.sendResponse(response)
      case "attach" =>
        // a response to an actual attach request sent by a DAP client
        attached.success(())
        super.sendResponse(response)
      case _ =>
        super.sendResponse(response)
    }
  }

  protected override def sendEvent(event: Events.DebugEvent): Unit = {
    try { super.sendEvent(event) }
    finally {
      if (event.`type` == "terminated") terminatedEvent.trySuccess(())
    }
  }

  private def name = runner.name


  private def cancelPromises(cause: Throwable): Unit = {
    debuggeeAddress.tryFailure(cause)
    attached.tryFailure(cause)
  }

  private object Callbacks extends DebugSessionCallbacks {
    def onListening(address: InetSocketAddress): Unit = {
      debuggeeAddress.success(address)
    }

    def printlnOut(msg: String): Unit = {
      val event = new OutputEvent(OutputEvent.Category.stdout, msg + System.lineSeparator())
      sendEvent(event)
    }

    def printlnErr(msg: String): Unit = {
      val event = new OutputEvent(OutputEvent.Category.stderr, msg + System.lineSeparator())
      sendEvent(event)
    }
  }
}

object DebugSession {
  sealed trait ExitStatus
  
  /**
    * The debugger has asked for a restart
    */
  final case object Restarted extends ExitStatus
  
  /**
    * The debugger has disconnected
    */
  final case object Disconnected extends ExitStatus 
  
  /**
    * The debuggee has terminated
    */
  final case object Terminated extends ExitStatus

  sealed trait State
  final case object Ready extends State
  final case class Started(debuggee: CancelableFuture[Unit]) extends State
  final case object Stopped extends State

  def apply(
      socket: Socket,
      runner: DebuggeeRunner,
      logger: Logger,
      autoClose: Boolean,
      gracePeriod: Duration
  )(implicit executionContext: ExecutionContext): DebugSession = {
    val adapter = new LoggerAdapter(logger)
    val context = DebugExtensions.newContext(runner)
    new DebugSession(socket, runner, context, logger, adapter, autoClose, gracePeriod)
  }

  private def toAttachRequest(seq: Int, address: InetSocketAddress): Request = {
    val arguments = new AttachArguments
    arguments.hostName = address.getHostName
    arguments.port = address.getPort

    val json = JsonUtils.toJsonTree(arguments, classOf[AttachArguments])
    new Request(seq, Command.ATTACH.getName, json.getAsJsonObject)
  }

  private def failed(request: Request, message: String): Response = {
    new Response(request.seq, request.command, false, message)
  }

  private def shouldRestart(disconnectRequest: Request): Boolean = {
    Try(JsonUtils.fromJson(disconnectRequest.arguments, classOf[DisconnectArguments]))
      .map(_.restart)
      .getOrElse(false)
  }

  private def loggerFactory(handler: LoggerAdapter): LoggerFactory = { name =>
    val logger = JLogger.getLogger(name)
    logger.getHandlers.foreach(logger.removeHandler)
    logger.setUseParentHandlers(false)
    if (name == Configuration.LOGGER_NAME) logger.addHandler(handler)
    logger
  }

  private final class LoggerAdapter(logger: Logger) extends Handler {
    /**
     * DAP server tends to send a lot of SocketClosed exceptions when the Debuggee process has exited. This helps us filter those logs
     */
    @volatile private var closingSession = false

    override def publish(record: LogRecord): Unit = {
      val message = record.getMessage
      record.getLevel match {
        case Level.INFO | Level.CONFIG => logger.info(message)
        case Level.WARNING => logger.warn(message)
        case Level.SEVERE =>
          if (isExpectedDuringCancellation(message) || isIgnoredError(message)) {
            logger.debug(message)
          } else {
            logger.error(message)
          }
        case _ => logger.debug(message)
      }
    }

    private final val socketClosed = "java.net.SocketException: Socket closed"
    private def isExpectedDuringCancellation(message: String): Boolean = {
      message.endsWith(socketClosed) && closingSession
    }

    private final val recordingWhenVmDisconnected =
      "Exception on recording event: com.sun.jdi.VMDisconnectedException"
    private def isIgnoredError(message: String): Boolean = {
      message.startsWith(recordingWhenVmDisconnected)
    }

    def onClosingSession(): Unit = {
      closingSession = true
    }

    override def flush(): Unit = ()
    override def close(): Unit = ()
  }
}
