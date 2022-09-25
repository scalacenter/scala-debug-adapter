package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.testing.TestDebugClient
import utest._

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

object MoreScala3DebugTests extends TestSuite {
  // the server needs only one thread for delayed responses of the launch and configurationDone requests
  val executorService = Executors.newFixedThreadPool(1)
  implicit val ec = ExecutionContext.fromExecutorService(executorService)

  def tests: Tests = Tests {
    "should support breakpoints in scala 3 with brace-less syntax" - {
      val debuggee = MainDebuggee.scala3Braceless()
      val server = getDebugServer(debuggee)
      val client = TestDebugClient.connect(server.uri)
      try {
        server.connect()
        client.initialize()
        client.launch()

        val breakpoints =
          client.setBreakpoints(debuggee.sourceFiles.head, Array(5, 7, 11))
        assert(breakpoints.size == 3)
        assert(breakpoints.forall(_.verified))

        client.configurationDone()
        val stop = client.stopped()
        val threadId = stop.threadId

        client.continue(threadId)
        client.stopped()

        client.continue(threadId)
        client.stopped()

        client.continue(threadId)
        client.exited()
        client.terminated()
      } finally {
        server.close()
        client.close()
      }
    }

    "should support breakpoints in scala 3 with @main" - {
      val debuggee = MainDebuggee.scala3MainAnnotation()
      val server = getDebugServer(debuggee)
      val client = TestDebugClient.connect(server.uri)
      try {
        server.connect()
        client.initialize()
        client.launch()

        val breakpoints =
          client.setBreakpoints(debuggee.sourceFiles.head, Array(4, 6, 10))
        assert(breakpoints.size == 3)
        assert(breakpoints.forall(_.verified))

        client.configurationDone()
        val stop = client.stopped()
        val threadId = stop.threadId

        client.continue(threadId)
        client.stopped()

        client.continue(threadId)
        client.stopped()

        client.continue(threadId)
        client.exited()
        client.terminated()
      } finally {
        server.close()
        client.close()
      }
    }
  }

  def getDebugServer(debuggee: Debuggee, logger: Logger = NoopLogger): DebugServer = {
    val tools = DebugTools(debuggee, ScalaInstanceResolver, NoopLogger)
    DebugServer(debuggee, tools, NoopLogger, testMode = true)
  }
}
