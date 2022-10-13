package ch.epfl.scala.debugadapter.testfmk

import utest.*
import ch.epfl.scala.debugadapter.DebugTools
import ch.epfl.scala.debugadapter.NoopLogger
import ch.epfl.scala.debugadapter.DebugServer
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import com.microsoft.java.debug.core.protocol.Types.StackFrame
import ch.epfl.scala.debugadapter.GithubUtils
import ch.epfl.scala.debugadapter.Debuggee
import scala.concurrent.duration.*
import ch.epfl.scala.debugadapter.Logger

abstract class DebugTestSuite extends TestSuite {
  // the server needs only one thread for delayed responses of the launch and configurationDone requests
  val executorService = Executors.newFixedThreadPool(1)
  implicit val ec = ExecutionContext.fromExecutorService(executorService)

  def isScala3(implicit debuggee: TestingDebuggee) = debuggee.scalaVersion.isScala3
  def isScala2(implicit debuggee: TestingDebuggee) = debuggee.scalaVersion.isScala2
  def isScala213(implicit debuggee: TestingDebuggee) = debuggee.scalaVersion.isScala213
  def isScala212(implicit debuggee: TestingDebuggee) = debuggee.scalaVersion.isScala212
  def isScala30(implicit debuggee: TestingDebuggee) = debuggee.scalaVersion.isScala30
  def isScala32(implicit debuggee: TestingDebuggee) = debuggee.scalaVersion.isScala32

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

  def check(steps: DebugStepAssert[?]*)(implicit debuggee: TestingDebuggee) = {
    val server = getDebugServer(debuggee)
    val client = TestDebugClient.connect(server.uri)
    try {
      server.connect()
      client.initialize()
      client.launch()

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
      def assertStop(assertion: StackFrame => Unit): Unit = {
        val stopped = client.stopped()
        threadId = stopped.threadId
        val stackTrace = client.stackTrace(threadId)
        topFrame = stackTrace.stackFrames.head
        assertion(topFrame)
      }
      steps.foreach {
        case DebugStepAssert(_: Breakpoint, assertion) =>
          if (threadId != -1) client.continue(threadId)
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
        case DebugStepAssert(_: NoStep, _) => ()
      }
      client.continue(threadId)

      // This is flaky, terminated can happen before exited
      if (!GithubUtils.isCI()) {
        client.exited()
        client.terminated()
      }
    } finally {
      server.close()
      client.close()
    }
  }
}
