package dap

import com.microsoft.java.debug.core.adapter.{IProviderContext, ProtocolServer => DapServer}
import com.microsoft.java.debug.core.protocol.Events.OutputEvent
import com.microsoft.java.debug.core.protocol.Messages.{Request, Response}
import com.microsoft.java.debug.core.protocol.Requests._
import com.microsoft.java.debug.core.protocol.{Events, JsonUtils}
import com.microsoft.java.debug.core.{Configuration, LoggerFactory}
import dap.DebugSession.Started
import dap.TimeoutScheduler._

import java.net.{InetSocketAddress, Socket}
import java.util.concurrent.{CancellationException, TimeUnit, TimeoutException}
import java.util.concurrent.atomic.AtomicBoolean
import java.util.logging.{Handler, Level, LogRecord, Logger => JLogger}
import scala.collection.mutable
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

/**
 *  This debug adapter maintains the lifecycle of the debuggee in separation from JDI.
 *  The debuggee is started/closed together with the session.
 *
 *  This approach makes it necessary to handle the "launch" requests as the "attach" ones.
 *  The JDI address of the debuggee is obtained through the [[DebugSessionCallbacks]]
 */
final class DebugSession(
    socket: Socket,
    initialDebugState: DebugSession.State,
    context: IProviderContext,
    initialLogger: Logger,
    loggerAdapter: DebugSession.LoggerAdapter
)(implicit executionContext: ExecutionContext) extends DapServer(
  socket.getInputStream,
  socket.getOutputStream,
  context,
  DebugSession.loggerFactory(loggerAdapter)
) {
  private type LaunchId = Int

  // A set of all processed launched requests by the client
  private val launchedRequests = mutable.Set.empty[LaunchId]

  private val isDisconnected: AtomicBoolean = new AtomicBoolean(false)
  private val endOfConnection = Promise[Unit]()
  private val debugAddress = Promise[InetSocketAddress]()
  
  private val sessionStatusPromise = Promise[DebugSession.ExitStatus]()
  private val attachedPromise = Promise[Unit]()
  private val debugState = new Synchronized(initialDebugState)

  def currentState: DebugSession.State = debugState.value
  def getDebugAddress: Future[InetSocketAddress] = debugAddress.future

  /**
   * Schedules the start of the debugging session.
   *
   * For a session to start, two executions must happen independently in a
   * non-blocking way: the debuggee process is started in the background and
   * the DAP server starts listening to client requests in an IO thread.
   */
  def start(): Unit = {
    debugState.transform {
      case DebugSession.Ready(runner) =>
        Future(run()) // start listening
        val debuggee = runner.run(Callbacks)
        debuggee.future()
          .failed
          .foreach(cancelPromises)
        DebugSession.Started(debuggee)

      case otherState =>
        otherState // don't start if already started or cancelled
    }
  }

  override def dispatchRequest(request: Request): Unit = {
    val requestId = request.seq
    request.command match {
      case "launch" =>
        // launch request is implemented by spinning up a JVM
        // and sending an attach request to the java DapServer
        launchedRequests.add(requestId)
        debugAddress
          .timeout(FiniteDuration(15, TimeUnit.SECONDS))
          .future
          .onComplete {
            case Success(address) =>
              super.dispatchRequest(DebugSession.toAttachRequest(requestId, address))
            case Failure(exception) =>
              val cause = s"Could not start debuggee due to: ${exception.getMessage}"
              this.sendResponse(DebugSession.failed(request, cause))
              attachedPromise.tryFailure(new IllegalStateException(cause))
              ()
          }

      case "configurationDone" =>
        // Delay handling of this request until we attach to the debuggee.
        // Otherwise, a race condition may happen when we try to communicate
        // with the VM we are not connected to
        attachedPromise.future
          .onComplete {
            case Success(_) =>
              super.dispatchRequest(request)
            case Failure(exception) =>
              sendResponse(DebugSession.failed(request, exception.getMessage))
          }

      case "disconnect" =>
        if (DebugSession.shouldRestart(request)) {
          sessionStatusPromise.trySuccess(DebugSession.Restarted)
        }

        val dispatchRequest = debugState.run {
          case DebugSession.Started(debuggee) =>
            cancelPromises(new CancellationException("Client disconnected"))
            cancelDebuggee(debuggee)
            (DebugSession.Cancelled, true)
          case otherState => (otherState, false)
        }
        
        if (dispatchRequest) {
          super.dispatchRequest(request)
        } else {
          // TODO clarify why request is not dispatched
          val ack = new Response(request.seq, request.command, true)
          sendResponse(ack)
        }
      
      case _ => super.dispatchRequest(request)
    }
  }

  override def sendResponse(response: Response): Unit = {
    val requestId = response.request_seq
    response.command match {
      case "attach" if launchedRequests(requestId) =>
        // attach response from java DapServer is transformed into a launch response
        // that is forwarded to the Bloop DAP client
        response.command = Command.LAUNCH.getName
        attachedPromise.success(())
        super.sendResponse(response)
      case "attach" =>
        // a response to an actual attach request sent by a Bloop DAP client
        attachedPromise.success(())
        super.sendResponse(response)
      case "disconnect" =>
        // we are sending a response manually but the actual handler is also sending one so let's ignore it
        // because our disconnection must be successful as it is basically just cancelling the debuggee
        if (isDisconnected.compareAndSet(false, true)) {
          super.sendResponse(response)
        }
      case _ =>
        super.sendResponse(response)
    }
  }

  override def sendEvent(event: Events.DebugEvent): Unit = {
    try { super.sendEvent(event) }
    finally {
      event.`type` match {
        case "exited" => loggerAdapter.onDebuggeeFinished()
        case "terminated" => endOfConnection.trySuccess(()) // Might already be set when canceling
        case _ => ()
      }
    }
  }

  /**
   * Completed, once this session exit status can be determined.
   * Those are: [[DebugSession.Terminated]] and [[DebugSession.Restarted]].
   * <p>Session gets the Terminated status when the communication stops without
   * the client ever requesting a restart.</p>
   * <p>Session becomes Restarted immediately when the restart request is received.
   * Note that the debuggee is still running and the communication with the client continues
   * (i.e. sending terminated and exited events).</p>
   */
  def exitStatus: Future[DebugSession.ExitStatus] =
    sessionStatusPromise.future

  /**
   * Cancels the background debuggee process, the DAP server and closes the socket.
   */
  def cancel(gracePeriod: Duration): Unit = {
    debugState.transform {
      case DebugSession.Ready(_) =>  
        socket.close()
        DebugSession.Cancelled

      case DebugSession.Started(debuggee) =>
        cancelPromises(new CancellationException("Debug session cancelled"))
        cancelDebuggee(debuggee)
      
        // Close connection after the grace period to prevent blocking if [[TerminalEvents]] are not sent
        try Await.result(endOfConnection.future, gracePeriod)
        catch {
          case _: TimeoutException =>
            val msg = "Communication with DAP client is frozen, closing client forcefully..."
            initialLogger.warn(msg)
        }
        socket.close()

        DebugSession.Cancelled

      case state => state
    }
  }

  private def cancelDebuggee(debuggee: CancelableFuture[Unit]): Unit = {
    loggerAdapter.onDebuggeeFinished()
    debuggee.cancel()
  }

  private def cancelPromises(cause: Throwable): Unit = {
    debugAddress.tryFailure(cause)
    attachedPromise.tryFailure(cause)
    ()
  }

  private object Callbacks extends DebugSessionCallbacks {
    def onListening(address: InetSocketAddress): Unit = {
      debugAddress.success(address)
    }

    def printOut(msg: String): Unit = {
      val event = new OutputEvent(OutputEvent.Category.stdout, msg + System.lineSeparator())
      sendEvent(event)
    }

    def printErr(msg: String): Unit = {
      val event = new OutputEvent(OutputEvent.Category.stderr, msg + System.lineSeparator())
      sendEvent(event)
    }
  }
}

object DebugSession {
  sealed trait ExitStatus
  final case object Restarted extends ExitStatus
  final case object Terminated extends ExitStatus

  sealed trait State
  final case class Ready(runner: DebuggeeRunner) extends State
  final case class Started(debuggee: CancelableFuture[Unit]) extends State
  final case object Cancelled extends State

  def apply(
      socket: Socket,
      runner: DebuggeeRunner,
      logger: Logger
  )(implicit executionContext: ExecutionContext): DebugSession = {
    val adapter = new LoggerAdapter(logger)
    val context = DebugExtensions.newContext(runner)
    val initialState = Ready(runner)
    new DebugSession(socket, initialState, context, logger, adapter)
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
     * Debuggee tends to send a lot of SocketClosed exceptions when bloop is terminating the socket. This helps us filter those logs
     */
    @volatile private var debuggeeFinished = false

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
      message.endsWith(socketClosed) && debuggeeFinished
    }

    private final val recordingWhenVmDisconnected =
      "Exception on recording event: com.sun.jdi.VMDisconnectedException"
    private def isIgnoredError(message: String): Boolean = {
      message.startsWith(recordingWhenVmDisconnected)
    }

    def onDebuggeeFinished(): Unit = {
      debuggeeFinished = true
    }

    override def flush(): Unit = ()
    override def close(): Unit = ()
  }
}
