package ch.epfl.scala.debugadapter.testfmk

import com.google.gson.JsonObject
import com.google.gson.internal.LinkedTreeMap
import com.microsoft.java.debug.core.protocol.Events._
import com.microsoft.java.debug.core.protocol.Requests._
import com.microsoft.java.debug.core.protocol.Responses._
import com.microsoft.java.debug.core.protocol.Types._
import com.microsoft.java.debug.core.protocol._

import java.io._
import java.net._
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit, TimeoutException}
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import scala.reflect._
import scala.util.control.NonFatal
import scala.concurrent.ExecutionContext
import scala.concurrent.Promise
import scala.concurrent.Await
import ch.epfl.scala.debugadapter.Logger

class TestingDebugClient(socket: Socket, logger: Logger)(implicit
    ec: ExecutionContext
) extends AbstractDebugClient(
      socket.getInputStream,
      socket.getOutputStream,
      logger
    ) {
  override def close(): Unit = {
    super.close()
    socket.close()
  }

  def initialize(timeout: Duration = 8.seconds): Messages.Response = {
    val args = new InitializeArguments()
    args.linesStartAt1 = true
    args.columnsStartAt1 = true
    val request = createRequest(Command.INITIALIZE, args)
    sendRequest(request, timeout)
  }

  def launch(timeout: Duration = 8.seconds): Messages.Response = {
    val request = createRequest(Command.LAUNCH, new LaunchArguments())
    sendRequest(request, timeout)
  }

  def attach(
      hostName: String,
      port: Int,
      timeout: Duration = 1.second
  ): Messages.Response = {
    val arguments = new AttachArguments()
    arguments.hostName = hostName
    arguments.port = port
    val request = createRequest(Command.ATTACH, arguments)
    sendRequest(request, timeout)
  }

  def configurationDone(timeout: Duration = 1.second): Messages.Response = {
    val request =
      createRequest(Command.CONFIGURATIONDONE, new ConfigurationDoneArguments())
    sendRequest(request, timeout)
  }

  def continue(
      threadId: Long,
      timeout: Duration = 8.seconds
  ): Messages.Response = {
    val args = new ContinueArguments()
    args.threadId = threadId
    val request = createRequest(Command.CONTINUE, args)
    sendRequest(request, timeout)
  }

  def setBreakpoints(
      source: Path,
      lines: Seq[Int],
      timeout: Duration = 1.second
  ): Array[Types.Breakpoint] = {
    val breakpoints = lines.map(l => new SourceBreakpoint(l, null, null))
    setSourceBreakpoints(source, breakpoints, timeout)
  }

  def setSourceBreakpoints(
      source: Path,
      breakpoints: Seq[SourceBreakpoint],
      timeout: Duration = 1.second
  ): Array[Types.Breakpoint] = {
    val args = new SetBreakpointArguments()
    args.source = new Types.Source(source.toString, 0)
    args.breakpoints = breakpoints.toArray
    val request = createRequest(Command.SETBREAKPOINTS, args)
    val response = sendRequest(request, timeout)
    getBody[SetBreakpointsResponseBody](response).breakpoints
  }

  def setBreakpointsInClass(
      className: String,
      lines: Array[Int],
      timeout: Duration = 1.second
  ): Array[Types.Breakpoint] = {
    val args = new SetBreakpointArguments()
    val source = new Types.Source()
    source.name = className
    source.path = "dap-fqcn:" + className
    source.sourceReference = 0
    args.source = source
    args.breakpoints = lines.map(l => new SourceBreakpoint(l, null, null))
    val request = createRequest(Command.SETBREAKPOINTS, args)
    val response = sendRequest(request, timeout)
    getBody[SetBreakpointsResponseBody](response).breakpoints
  }

  def stackTrace(
      threadId: Long,
      timeout: Duration = 1.second
  ): StackTraceResponseBody = {
    val args = new StackTraceArguments()
    args.threadId = threadId
    val request = createRequest(Command.STACKTRACE, args)
    val response = sendRequest(request, timeout)
    getBody[StackTraceResponseBody](response)
  }

  def scopes(frameId: Int, timeout: Duration = 1.second): Array[Scope] = {
    val args = new ScopesArguments()
    args.frameId = frameId
    val request = createRequest(Command.SCOPES, args)
    val response = sendRequest(request, timeout)
    getBody[ScopesResponseBody](response).scopes
  }

  def variables(
      variablesReference: Int,
      timeout: Duration = 1.second
  ): Array[Variable] = {
    val args = new VariablesArguments()
    args.variablesReference = variablesReference
    val request = createRequest(Command.VARIABLES, args)
    val response = sendRequest(request, timeout)
    getBody[VariablesResponseBody](response).variables
  }

  def stepIn(threadId: Long, timeout: Duration = 1.second): Unit = {
    val args = new StepInArguments()
    args.threadId = threadId
    val request = createRequest(Command.STEPIN, args)
    val response = sendRequest(request, timeout)
  }

  def stepOut(threadId: Long, timeout: Duration = 1.second): Unit = {
    val args = new StepOutArguments()
    args.threadId = threadId
    val request = createRequest(Command.STEPOUT, args)
    val response = sendRequest(request, timeout)
  }

  def evaluate(
      expression: String,
      frameId: Int,
      timeout: Duration = 16.seconds
  ): Either[String, String] = {
    val args = new EvaluateArguments()
    args.expression = expression
    args.frameId = frameId
    args.context = "repl"
    val request = createRequest(Command.EVALUATE, args)
    val response = sendRequest(request, timeout)

    Option(getBody[EvaluateResponseBody](response).result)
      .toRight(getBody[ErrorResponseBody](response).error.format)
  }

  def disconnect(
      restart: Boolean,
      timeout: Duration = 1.second
  ): Messages.Response = {
    val args = new DisconnectArguments()
    args.restart = restart
    val request = createRequest(Command.DISCONNECT, args)
    sendRequest(request, timeout)
  }

  def initialized(timeout: Duration = 1.second): InitializedEvent = {
    val event = receiveEvent(timeout)(e => e.event == "initialized")
    getBody[InitializedEvent](event)
  }

  def terminated(timeout: Duration = 1.second): TerminatedEvent = {
    val event = receiveEvent(timeout)(e => e.event == "terminated")
    getBody[TerminatedEvent](event)
  }

  def exited(timeout: Duration = 4.seconds): ExitedEvent = {
    val event = receiveEvent(timeout)(e => e.event == "exited")
    getBody[ExitedEvent](event)
  }

  def outputed(
      f: OutputEvent => Boolean,
      timeout: Duration = 1.second
  ): OutputEvent = {
    val event = receiveEvent(timeout) { e =>
      if (e.event == "output") {
        val output = getBody[OutputEvent](e)
        f(output)
      } else false
    }
    getBody[OutputEvent](event)
  }

  def stopped(timeout: Duration = 16.seconds): StoppedEvent = {
    val event = receiveEvent(timeout)(e => e != null && e.event == "stopped")
    getBody[StoppedEvent](event)
  }

  private def getBody[T](e: Messages.Response)(implicit tag: ClassTag[T]): T = {
    val tree = JsonUtils.toJsonTree(e.body, classOf[LinkedTreeMap[_, _]])
    JsonUtils.fromJson(tree, tag.runtimeClass.asInstanceOf[Class[T]])
  }

  private def getBody[T](e: Messages.Event)(implicit tag: ClassTag[T]): T = {
    val tree = JsonUtils.toJsonTree(e.body, classOf[LinkedTreeMap[_, _]])
    JsonUtils.fromJson(tree, tag.runtimeClass.asInstanceOf[Class[T]])
  }

  private def createRequest[T](command: Requests.Command, args: T)(implicit
      tag: ClassTag[T]
  ): Messages.Request = {
    val json =
      JsonUtils.toJsonTree(args, tag.runtimeClass).asInstanceOf[JsonObject]
    new Messages.Request(command.getName, json)
  }
}

object TestingDebugClient {
  def connect(
      uri: URI,
      timeout: Duration = 4.seconds,
      logger: Logger = NoopLogger
  )(implicit ec: ExecutionContext): TestingDebugClient = {
    val socket = new Socket()
    val address = new InetSocketAddress(uri.getHost, uri.getPort)
    socket.connect(address, timeout.toMillis.intValue)
    val client = new TestingDebugClient(socket, logger)
    val listening = new java.lang.Thread {
      override def run(): Unit = client.run()
    }
    listening.start()
    client
  }
}

class AbstractDebugClient(
    input: InputStream,
    output: OutputStream,
    logger: Logger
) {
  private final val BufferSize = 4096
  private final val TwoCRLF = "\r\n\r\n"
  private val ContentLengthMatcher = "Content-Length: (\\d+)".r
  // vscode protocol uses UTF-8 as encoding format.
  private val ProtocolEncoding = StandardCharsets.UTF_8

  protected var terminateSession = false

  private val reader: Reader = new BufferedReader(
    new InputStreamReader(input, ProtocolEncoding)
  )
  private val writer: Writer = new PrintWriter(
    new BufferedWriter(new OutputStreamWriter(output, ProtocolEncoding))
  )

  private val sequenceNumber = new AtomicInteger(1)

  private var responsePromise: Promise[Messages.Response] = _
  private val events = new LinkedBlockingQueue[Messages.Event]()

  def close(): Unit = {
    terminateSession = true
    input.close()
    output.close()
  }

  def run(): Unit = {
    var received = ""
    val buffer = new Array[Char](BufferSize)

    while (!terminateSession) {
      try {
        val read = reader.read(buffer, 0, BufferSize)

        if (read == -1) {
          terminateSession = true
        } else {
          received = received + new String(buffer.take(read))
          received = processData(received)
        }
      } catch {
        case _: IOException =>
          terminateSession = true
      }
    }
  }

  def sendRequest(
      request: Messages.Request,
      timeout: Duration
  ): Messages.Response = {
    responsePromise = Promise[Messages.Response]()
    sendMessage(request)
    Await.result(responsePromise.future, timeout)
  }

  def receiveEvent(
      timeout: Duration
  )(f: Messages.Event => Boolean): Messages.Event = {
    Iterator
      .continually(events.poll(timeout.toMillis, TimeUnit.MILLISECONDS))
      .map(e => if (e == null) throw new TimeoutException() else e)
      .filter(f)
      .next()
  }

  private def processData(received: String): String = {
    var remaining = received
    val rawMessages =
      Iterator
        .continually {
          findFirstMessage(remaining).map { case (begin, end) =>
            val rawMessage = remaining.substring(begin, end)
            remaining = remaining.substring(end)
            rawMessage
          }
        }
        .takeWhile(_.nonEmpty)
        .flatten
    rawMessages.foreach { raw =>
      try {
        logger.debug(s"Received $raw")
        val message = JsonUtils.fromJson(raw, classOf[Messages.ProtocolMessage])

        if (message.`type`.equals("response")) {
          val response = JsonUtils.fromJson(raw, classOf[Messages.Response])
          responsePromise.success(response)
        } else if (message.`type`.equals("event")) {
          val event = JsonUtils.fromJson(raw, classOf[Messages.Event])
          events.put(event)
        }
      } catch {
        case NonFatal(e) =>
          System.err.println(s"Error parsing message: ${e.getMessage}")
      }
    }
    remaining
  }

  private def findFirstMessage(received: String): Option[(Int, Int)] = {
    for {
      beginIdx <-
        received.indexOf(TwoCRLF) match {
          case -1 => None
          case i => Some(i + TwoCRLF.length)
        }
      firstMatch <- ContentLengthMatcher.findFirstMatchIn(received)
      contentLength = firstMatch.group(1).toInt
      endIdx <-
        if (received.length >= beginIdx + contentLength)
          Some(beginIdx + contentLength)
        else None
    } yield (beginIdx, endIdx)
  }

  private def sendMessage(message: Messages.ProtocolMessage): Unit = {
    message.seq = this.sequenceNumber.getAndIncrement()

    val jsonMessage = JsonUtils.toJson(message)
    val jsonBytes = jsonMessage.getBytes(ProtocolEncoding)

    val header: String = s"Content-Length: ${jsonBytes.length}$TwoCRLF"
    val headerBytes = header.getBytes(ProtocolEncoding)

    val data = new String(headerBytes ++ jsonBytes, ProtocolEncoding)

    try {
      this.writer.write(data)
      this.writer.flush()
      logger.debug(s"Sent $jsonMessage")
    } catch {
      case NonFatal(e) =>
        System.err.println(s"Write data to io exception: ${e.getMessage}")
    }
  }
}
