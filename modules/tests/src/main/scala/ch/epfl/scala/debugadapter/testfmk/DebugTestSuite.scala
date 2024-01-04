package ch.epfl.scala.debugadapter.testfmk

import ch.epfl.scala.debugadapter.DebugServer
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import com.microsoft.java.debug.core.protocol.Types.StackFrame
import ch.epfl.scala.debugadapter.GithubUtils
import ch.epfl.scala.debugadapter.Debuggee
import scala.concurrent.duration.*
import ch.epfl.scala.debugadapter.Logger
import java.net.URI
import com.microsoft.java.debug.core.protocol.Events.OutputEvent.Category
import munit.FunSuite
import munit.Assertions.*
import com.microsoft.java.debug.core.protocol.Types.SourceBreakpoint
import ch.epfl.scala.debugadapter.DebugConfig
import scala.concurrent.Future
import scala.concurrent.Await
import com.microsoft.java.debug.core.protocol.Types.Variable
import scala.util.Properties

case class DebugCheckState(
    client: TestingDebugClient,
    threadId: Long,
    topFrame: StackFrame,
    paused: Boolean
)

abstract class DebugTestSuite extends FunSuite with DebugTest {
  override def munitTimeout: Duration = 120.seconds
}

trait DebugTest {
  // the server needs only one thread for delayed responses of the launch and configurationDone requests
  val executorService = Executors.newFixedThreadPool(5)
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(executorService)

  protected def defaultConfig: DebugConfig = DebugConfig.default.copy(autoCloseSession = false, testMode = true)

  def javaVersion: String = Properties.javaVersion

  def isJava8 = javaVersion.contains("1.8")
  def isScala3(implicit ctx: TestingContext) = ctx.scalaVersion.isScala3
  def isScala2(implicit ctx: TestingContext) = ctx.scalaVersion.isScala2
  def isScala213(implicit ctx: TestingContext) = ctx.scalaVersion.isScala213
  def isScala212(implicit ctx: TestingContext) = ctx.scalaVersion.isScala212
  def isScala30(implicit ctx: TestingContext) = ctx.scalaVersion.isScala30
  def isScala31Plus(implicit ctx: TestingContext) = ctx.scalaVersion.isScala31Plus
  def isScala33(implicit ctx: TestingContext) = ctx.scalaVersion.isScala33

  def getDebugServer(
      debuggee: Debuggee,
      config: DebugConfig = defaultConfig,
      logger: Logger = NoopLogger
  ): DebugServer =
    DebugServer(debuggee, TestingResolver, logger, config = config)

  def startDebugServer(
      debuggee: Debuggee,
      gracePeriod: Duration = 2.seconds,
      logger: Logger = NoopLogger
  ): DebugServer.Handler =
    DebugServer.start(debuggee, TestingResolver, logger)

  def check(uri: URI, attach: Option[Int] = None)(steps: DebugStepAssert*): Unit = {
    val client = TestingDebugClient.connect(uri)
    try endDebugSession(check(client, attach)(steps*))
    finally client.close()
  }

  def init(uri: URI)(steps: DebugStepAssert*): DebugCheckState = {
    val client = TestingDebugClient.connect(uri)
    check(client, None)(steps*)
  }

  def check(config: DebugConfig)(steps: DebugStepAssert*)(implicit debuggee: TestingDebuggee): Unit = {
    val server = getDebugServer(debuggee, config = config)
    val client = TestingDebugClient.connect(server.uri)
    try {
      server.connect()
      endDebugSession(check(client, None)(steps*))
    } finally {
      client.close()
      server.close()
    }
  }

  def check(steps: DebugStepAssert*)(implicit debuggee: TestingDebuggee): Unit = {
    val server = getDebugServer(debuggee)
    val client = TestingDebugClient.connect(server.uri)
    try {
      server.connect()
      endDebugSession(check(client, None)(steps*))
    } finally {
      client.close()
      server.close()
    }
  }

  private def check(client: TestingDebugClient, attach: Option[Int])(
      steps: DebugStepAssert*
  ): DebugCheckState = {
    client.initialize()
    attach match {
      case Some(port) => client.attach("localhost", port)
      case None => client.launch()
    }

    val sourceBreakpoints = steps
      .collect { case s: SingleStepAssert[?] => s.step }
      .distinct
      .collect {
        case b: Breakpoint =>
          b.sourceFile -> new SourceBreakpoint(b.line, b.condition.getOrElse(null), null)
        case l: Logpoint =>
          val breakpoint = new SourceBreakpoint(l.line, null, null)
          breakpoint.logMessage = l.logMessage
          l.sourceFile -> breakpoint
      }

    sourceBreakpoints
      .groupBy(_._1)
      .map { case (sourceFile, bs) => (sourceFile, bs.map(_._2)) }
      .foreach { case (sourceFile, sourceBreakpoints) =>
        sourceBreakpoints
          .groupBy(_.line)
          .foreach { case (line, bs) =>
            assert(bs.size == 1, s"More than one breakpoint in $sourceFile on line $line")
          }
        val configuredBreakpoints = client.setSourceBreakpoints(sourceFile, sourceBreakpoints)
        assertEquals(configuredBreakpoints.length, sourceBreakpoints.length)
        assert(configuredBreakpoints.forall(_.verified))
      }
    client.configurationDone()

    runChecks(DebugCheckState(client, -1, null, false))(steps)
  }

  def runChecks(state_in: DebugCheckState)(steps: Seq[DebugStepAssert]) = {
    var state = state_in

    def formatFrame(frame: StackFrame): String = {
      Option(frame.source) match {
        case None => s"${state.topFrame.name} ${state.topFrame.line}"
        case Some(source) => s"${source.name} ${state.topFrame.line}"
      }
    }

    def evaluateExpression(eval: Evaluation, assertion: Either[String, String] => Unit): Future[Unit] = {
      println(s"$$ ${eval.expression}")
      state.client.evaluate(eval.expression, state.topFrame.id).map { resp =>
        resp.foreach(res => println(s"> $res"))
        resp.left.foreach(err => println(s"> $err"))
        assertion(resp)
      }
    }

    def inspect(variable: LocalVariable, assertion: Array[Variable] => Unit): Unit = {
      val values = for {
        localScopeRef <- state.client.scopes(state.topFrame.id).find(_.name == "Local").map(_.variablesReference)
        variableRef <- state.client.variables(localScopeRef).find(_.name == variable.name).map(_.variablesReference)
      } yield state.client.variables(variableRef)
      assertion(values.getOrElse(throw new NoSuchElementException(variable.name)))
    }

    def assertStop(assertion: Array[StackFrame] => Unit): DebugCheckState = {
      val stopped = state.client.stopped()
      val threadId = stopped.threadId
      val stackTrace = state.client.stackTrace(threadId)

      assertion(stackTrace.stackFrames)
      state.copy(threadId = threadId, topFrame = stackTrace.stackFrames.head, paused = true)
    }

    steps.foreach {
      case SingleStepAssert(_: Breakpoint, assertion) =>
        state = continueIfPaused(state)
        state = assertStop(assertion)
      case SingleStepAssert(_: Logpoint, assertion) =>
        state = continueIfPaused(state)
        // A log point needs time for evaluation
        val event = state.client.outputed(m => m.category == Category.stdout, 16.seconds)
        print(s"> ${event.output}")
        assertion(event.output.trim)
      case SingleStepAssert(StepIn, assertion) =>
        println(s"Stepping in, at ${formatFrame(state.topFrame)}")
        state.client.stepIn(state.threadId)
        state = assertStop(assertion)
      case SingleStepAssert(StepOut, assertion) =>
        println(s"Stepping out, at ${formatFrame(state.topFrame)}")
        state.client.stepOut(state.threadId)
        state = assertStop(assertion)
      case SingleStepAssert(StepOver, assertion) =>
        println(s"Stepping over, at ${formatFrame(state.topFrame)}")
        state.client.stepOver(state.threadId)
        state = assertStop(assertion)
      case SingleStepAssert(eval: Evaluation, assertion) =>
        Await.result(evaluateExpression(eval, assertion), 16.seconds)
      case SingleStepAssert(Outputed, assertion) =>
        state = continueIfPaused(state)
        val event = state.client.outputed(m => m.category == Category.stdout)
        print(s"> ${event.output}")
        assertion(event.output.trim)
      case SingleStepAssert(NoStep, _) => ()
      case ParallelStepsAsserts(steps) =>
        val evaluations = steps.map { step =>
          evaluateExpression(
            step.step.asInstanceOf[Evaluation],
            step.assertion.asInstanceOf[Either[String, String] => Unit]
          )
        }
        Await.result(Future.sequence(evaluations), 64.seconds)
      case SingleStepAssert(localVariable: LocalVariable, assertion) =>
        inspect(localVariable, assertion)
      case SingleStepAssert(Custom(f), _) => f()
      case SingleStepAssert(RedefineClasses, _) =>
        state.client.redefineClasses()
        state = assertStop(_ => ())
    }

    state
  }

  def continueIfPaused(state: DebugCheckState): DebugCheckState =
    if (state.paused) {
      state.client.continue(state.threadId)
      state.copy(paused = false)
    } else state

  def endDebugSession(state: DebugCheckState): DebugCheckState = {
    val newState = continueIfPaused(state)
    // This is flaky, terminated can happen before exited
    if (!GithubUtils.isCI()) {
      newState.client.exited()
      newState.client.terminated()
    }
    newState
  }
}

object DebugTest extends DebugTest
