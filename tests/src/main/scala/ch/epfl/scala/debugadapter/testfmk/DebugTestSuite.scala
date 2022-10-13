package ch.epfl.scala.debugadapter.testfmk

import utest.*
import ch.epfl.scala.debugadapter.DebugTools
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

abstract class DebugTestSuite extends TestSuite with DebugTest

trait DebugTest {
  // the server needs only one thread for delayed responses of the launch and configurationDone requests
  val executorService = Executors.newFixedThreadPool(1)
  implicit val ec = ExecutionContext.fromExecutorService(executorService)

  def isScala3(implicit ctx: DebugContext) = ctx.scalaVersion.isScala3
  def isScala2(implicit ctx: DebugContext) = ctx.scalaVersion.isScala2
  def isScala213(implicit ctx: DebugContext) = ctx.scalaVersion.isScala213
  def isScala212(implicit ctx: DebugContext) = ctx.scalaVersion.isScala212
  def isScala30(implicit ctx: DebugContext) = ctx.scalaVersion.isScala30
  def isScala32(implicit ctx: DebugContext) = ctx.scalaVersion.isScala32

  def getDebugServer(
      debuggee: Debuggee,
      gracePeriod: Duration = 2.seconds,
      logger: Logger = NoopLogger
  ): DebugServer = {
    val tools = DebugTools(debuggee, TestingResolver, NoopLogger)
    DebugServer(debuggee, tools, NoopLogger, testMode = true)
  }

  def startDebugServer(
      debuggee: Debuggee,
      gracePeriod: Duration = 2.seconds,
      logger: Logger = NoopLogger
  ): DebugServer.Handler = {
    val tools = DebugTools(debuggee, TestingResolver, NoopLogger)
    DebugServer.start(debuggee, tools, NoopLogger)
  }

  def check(uri: URI, attach: Option[Int] = None)(steps: DebugStepAssert[?]*): Unit = {
    val client = TestDebugClient.connect(uri)
    try check(client, attach)(steps*)
    finally client.close()
  }

  def check(steps: DebugStepAssert[?]*)(implicit debuggee: TestingDebuggee): Unit = {
    val server = getDebugServer(debuggee)
    val client = TestDebugClient.connect(server.uri)
    try {
      server.connect()
      check(client, None)(steps*)
    } finally {
      client.close()
      server.close()
    }
  }

  private def check(client: TestDebugClient, attach: Option[Int])(steps: DebugStepAssert[?]*): Unit = {
    client.initialize()
    attach match {
      case Some(port) => client.attach("localhost", port)
      case None => client.launch()
    }

    val breakpoints = steps.map(_.step).collect { case b: Breakpoint => b }
    breakpoints
      .groupBy(_.sourceFile)
      .foreach { case (sourceFile, breakpoints) =>
        val conditionalBreakpoints = breakpoints
          .map(b => (b.line, b.condition.orNull))
          .distinct
        val configuredBreakpoints = client.setConditionalBreakpoints(sourceFile, conditionalBreakpoints)
        assert(configuredBreakpoints.length == conditionalBreakpoints.length)
        assert(configuredBreakpoints.forall(_.verified))
      }
    client.configurationDone()

    var threadId: Long = -1
    var topFrame: StackFrame = null
    var paused = false
    def continueIfPaused(): Unit = {
      if (paused) {
        client.continue(threadId)
        paused = false
      }
    }
    def assertStop(assertion: StackFrame => Unit): Unit = {
      val stopped = client.stopped()
      paused = true
      threadId = stopped.threadId
      val stackTrace = client.stackTrace(threadId)
      topFrame = stackTrace.stackFrames.head
      assertion(topFrame)
    }
    steps.foreach {
      case DebugStepAssert(_: Breakpoint, assertion) =>
        continueIfPaused()
        assertStop(assertion)
      case DebugStepAssert(_: StepIn, assertion) =>
        println(s"Stepping in, at ${topFrame.source.name} ${topFrame.line}")
        client.stepIn(threadId)
        assertStop(assertion)
      case DebugStepAssert(_: StepOut, assertion) =>
        println(s"Stepping out, at ${topFrame.source.name} ${topFrame.line}")
        client.stepOut(threadId)
        assertStop(assertion)
      case DebugStepAssert(_: StepOver, assertion) =>
        ???
      case DebugStepAssert(eval: Evaluation, assertion) =>
        println(s"$$ ${eval.expression}")
        val response = client.evaluate(eval.expression, topFrame.id)
        response.foreach(res => println(s" > $res"))
        assertion(response)
      case DebugStepAssert(Outputed(), assertion) =>
        continueIfPaused()
        val event = client.outputed(m => m.category == Category.stdout)
        assertion(event.output.trim)
      case DebugStepAssert(_: NoStep, _) => ()
    }
    continueIfPaused()

    // This is flaky, terminated can happen before exited
    if (!GithubUtils.isCI()) {
      client.exited()
      client.terminated()
    }
  }
}

object DebugTest extends DebugTest
