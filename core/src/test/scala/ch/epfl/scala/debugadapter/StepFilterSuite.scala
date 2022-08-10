package ch.epfl.scala.debugadapter

import utest._
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import ch.epfl.scala.debugadapter.testing.TestDebugClient
import scala.concurrent.duration._
import com.microsoft.java.debug.core.protocol.Types.StackFrame

abstract class StepFilterSuite(scalaVersion: ScalaVersion) extends TestSuite {
  // the server needs only one thread for delayed responses of the launch and configurationDone requests
  private val executorService = Executors.newFixedThreadPool(1)
  private implicit val ec =
    ExecutionContext.fromExecutorService(executorService)

  case class Breakpoint(line: Int)(val steps: Step*)

  sealed trait Step {
    val assertion: StackFrame => Unit
    def assert(frame: StackFrame): Unit = assertion(frame)
  }
  case class StepInto(assertion: StackFrame => Unit) extends Step
  case class StepOut(assertion: StackFrame => Unit) extends Step

  object StepInto {
    def line(expectedLine: Int): StepInto = {
      StepInto { frame =>
        val obtainedLine = frame.line
        assert(obtainedLine == expectedLine)
      }
    }

    def file(expectedName: String): StepInto = {
      StepInto { frame =>
        val obtainedName = frame.source.name
        assert(obtainedName == expectedName)
      }
    }

    def method(expectedName: String): StepInto = {
      StepInto { frame =>
        val obtainedName = frame.name
        assert(obtainedName == expectedName)
      }
    }
  }

  object StepOut {
    def line(expectedLine: Int): StepOut = {
      StepOut { frame =>
        val obtainedLine = frame.line
        assert(obtainedLine == expectedLine)
      }
    }
  }

  protected def assertInMainClass(
      source: String,
      mainClass: String
  )(breakpoints: Breakpoint*): Unit = {
    val runner =
      MainDebuggeeRunner.mainClassRunner(source, mainClass, scalaVersion)
    val server = DebugServer(runner, new DebugServer.Address(), NoopLogger)
    val client = TestDebugClient.connect(server.uri, 20.seconds)
    try {
      server.connect()
      client.initialize()
      client.launch()

      val lines = breakpoints.map(_.line).distinct.toArray
      val resp = client.setBreakpoints(runner.sourceFiles.head, lines)
      assert(resp.length == lines.length)
      assert(resp.forall(_.verified))
      client.configurationDone()

      breakpoints.foreach { breakpoint =>
        val stopped = client.stopped()
        assert(stopped.reason == "breakpoint")
        val threadId = stopped.threadId
        val stackTrace = client.stackTrace(threadId)
        assert(stackTrace.stackFrames.head.line == breakpoint.line)

        var currentLine = breakpoint.line
        breakpoint.steps.foreach { step =>
          step match {
            case StepInto(_) =>
              println(s"Stepping into, at line $currentLine")
              client.stepIn(threadId)
            case StepOut(_) =>
              println(s"Stepping out, at line $currentLine")
              client.stepOut(threadId)
          }
          client.stopped(8.seconds)
          val stackTrace = client.stackTrace(threadId)
          val topFrame = stackTrace.stackFrames.head
          currentLine = topFrame.line
          step.assert(topFrame)
        }
        client.continue(threadId)
      }

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
