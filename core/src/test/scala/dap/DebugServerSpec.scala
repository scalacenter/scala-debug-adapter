package dap

import com.microsoft.java.debug.core.protocol.Events.OutputEvent.Category
import sbt.io.IO
import utest._

import java.net.{ConnectException, SocketException, SocketTimeoutException}
import java.util.concurrent.TimeoutException
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

object DebugServerSpec extends TestSuite {
  private val DefaultTimeoutMillis = 2000
  /** the server needs two threads:
    * - the first one for listening
    * - the second one for delayed responses of the launch and configurationDone requests
    */
  val executorService  = Executors.newFixedThreadPool(2)
  implicit val ec = ExecutionContext.fromExecutorService(executorService)

  val lineSeparator = System.getProperty("line.separator")

  def tests: Tests = Tests {
    "should prevent connection when closed" - {
      val runner = new MockDebuggeeRunner()
      val server = new DebugServer(runner, NoopLogger)
      server.close()
      try  { 
        TestDebugClient.connect(server.uri, NoopLogger)
        assert(false) // connect was supposed to fail
      } catch {
        case _: SocketTimeoutException => ()
        case _: ConnectException => ()
        case e: SocketException =>
          val msg = e.getMessage
          assert(msg.endsWith("(Connection refused)") || msg.endsWith("(Connection Failed)"))
      }
    }

    "should start session when client connects" - {
      val runner = new MockDebuggeeRunner()
      val server = new DebugServer(runner, NoopLogger)
      val client = TestDebugClient.connect(server.uri, NoopLogger)
      try {
        val session = server.connect()
        assertMatch(session.currentState) {
          case DebugSession.Started(_) => ()
        }
      } finally {
        server.close()
        client.close()
      }
    }

    "should cancel session when closed" - {
      val runner = new MockDebuggeeRunner()
      val server = new DebugServer(runner, NoopLogger)
      val client = TestDebugClient.connect(server.uri, NoopLogger)
      try {
        val session = server.connect()
        server.close()
        assert(session.currentState == DebugSession.Cancelled)
      } finally {
        server.close()
        client.close()
      }
    }

    "should initialize only one connection" - {
      val runner = new MockDebuggeeRunner()
      val server = new DebugServer(runner, NoopLogger)
      val client1 = TestDebugClient.connect(server.uri, NoopLogger)
      try {
        server.connect()
        client1.initialize()
        
        var client2: TestDebugClient = null
        try {
          client2 = TestDebugClient.connect(server.uri, NoopLogger)
          client2.initialize()
          assert(false) // the server should not respond to the second client
        } catch {
          case _: SocketTimeoutException => ()
          case _: TimeoutException => ()
        } finally {
          if (client2 != null) client2.close()
        }

      } finally {
        server.close()
        client1.close()
      }
    }

    "should not launch if the debuggee has not started a jvm" - {
      val runner = new MockDebuggeeRunner()
      val server = new DebugServer(runner, NoopLogger)
      val client = TestDebugClient.connect(server.uri, NoopLogger)
      try {
        server.connect()
        client.initialize()
        try {
          client.launch()
          assert(false)
        } catch {
          case _: TimeoutException => ()
        }
      } finally {
        server.close()
        client.close()
      }
    }

    "should set debug address when debuggee starts the jvm" - {
      val tempDir = IO.temporaryDirectory
      val runner = MainDebuggeeRunner.sleep(tempDir, NoopLogger)
      val server = new DebugServer(runner, NoopLogger)
      val client = TestDebugClient.connect(server.uri, NoopLogger)
      try {
        val session = server.connect()
        Await.result(session.getDebugAddress, DefaultTimeoutMillis millis)
      } finally {
        server.close()
        client.close()
        IO.delete(tempDir)
      }
    }

    "should respond failure to launch request if debugee throws an exception" - {
      val runner = new MockDebuggeeRunner()
      val server = new DebugServer(runner, NoopLogger)
      val client = TestDebugClient.connect(server.uri, NoopLogger)
      try {
        server.connect()
        runner.currentProcess.stopped.failure(new Exception())
        client.initialize()
        assert(!client.launch().success)
      } finally {  
        server.close()
        client.close()
      }
    }

    "should launch and send initialized event if the debuggee has started a jvm" - {
      val tempDir = IO.createTemporaryDirectory
      val runner = MainDebuggeeRunner.sleep(tempDir)
      val server = new DebugServer(runner, NoopLogger)
      val client = TestDebugClient.connect(server.uri, NoopLogger)
      try {
        server.connect()
        client.initialize()
        assert(client.launch().success)
        client.initialized
      } finally {  
        server.close()
        client.close()
        IO.delete(tempDir)
      }
    }

    "should send terminated events when closed" - {
      val tempDir = IO.createTemporaryDirectory
      val runner = MainDebuggeeRunner.sleep(tempDir)
      val server = new DebugServer(runner, NoopLogger)
      val client = TestDebugClient.connect(server.uri, NoopLogger)
      try {
        server.connect()
        client.initialize()
        client.launch()
        client.configurationDone()
        // give some time for the debuggee to terminate gracefully
        server.close(DefaultTimeoutMillis millis)
        client.terminated
      } finally { 
        server.close() // in case test fails
        client.close()
        IO.delete(tempDir)
      }
    }

    "should send output event when debuggee prints to stdout" - {
      val tempDir = IO.createTemporaryDirectory
      val runner = MainDebuggeeRunner.helloWorld(tempDir)
      val server = new DebugServer(runner, NoopLogger)
      val client = TestDebugClient.connect(server.uri, NoopLogger, timeout = DefaultTimeoutMillis millis)
      try {
        server.connect()
        client.initialize()
        client.launch()
        client.configurationDone()
        val outputEvent = client.outputed(_.category == Category.stdout)
        assert(outputEvent.output == s"Hello, World!$lineSeparator")
      } finally {
        server.close()
        client.close()
        IO.delete(tempDir)
      }
    }

    "should send exited and terminated events when debuggee exits successfully" - {
      val tempDir = IO.createTemporaryDirectory
      val runner = MainDebuggeeRunner.helloWorld(tempDir)
      val server = new DebugServer(runner, NoopLogger)
      val client = TestDebugClient.connect(server.uri, NoopLogger, timeout = DefaultTimeoutMillis millis)
      try {
        server.connect()
        client.initialize()
        client.launch()
        client.configurationDone()
        // the debuggee should exits immediately
        val exitedEvent = client.exited
        assert(exitedEvent.exitCode == 0)

        client.terminated
      } finally {
        server.close()
        client.close()
        IO.delete(tempDir)
      }
    }

    "should send exited and terminated events when debuggee exits in error" - {
      val tempDir = IO.createTemporaryDirectory
      val runner = MainDebuggeeRunner.sysExit(tempDir)
      val server = new DebugServer(runner, NoopLogger)
      val client = TestDebugClient.connect(server.uri, NoopLogger)
      try {
        server.connect()
        client.initialize()
        client.launch()
        client.configurationDone()
        // the debuggee should exits immediately
        val exitedEvent = client.exited

        // surprisingly, the java debug implementation always set exitCode to 0
        // https://github.com/microsoft/java-debug/blob/211c47aabec6d47d8393ec39b6fdf0cbfcd8dbb0/com.microsoft.java.debug.core/src/main/java/com/microsoft/java/debug/core/adapter/handler/ConfigurationDoneRequestHandler.java#L84
        assert(exitedEvent.exitCode == 0)

        client.terminated
      } finally {
        server.close()
        client.close()
        IO.delete(tempDir)
      }
    }

    "should support scala breakpoints" - {
      val tempDir = IO.createTemporaryDirectory
      val runner = MainDebuggeeRunner.scalaBreakpointTest(tempDir)
      val server = new DebugServer(runner, NoopLogger)
      val client = TestDebugClient.connect(server.uri, NoopLogger)
      try {
        server.connect()
        client.initialize()
        client.launch()
        
        val breakpoints = client.setBreakpoints(runner.source, Array(3, 11, 18, 12, 7))
        assert(breakpoints.forall(_.verified))
        
        client.configurationDone()
        val stopped1 = client.stopped
        val threadId = stopped1.threadId
        assert(stopped1.reason == "breakpoint")
        
        client.continue(threadId)
        val stopped2 = client.stopped
        assert(stopped2.reason == "breakpoint")
        assert(stopped2.threadId == threadId)
        
        client.continue(threadId)
        val stopped3 = client.stopped
        assert(stopped3.reason == "breakpoint")
        assert(stopped3.threadId == threadId)
        
        client.continue(threadId)
        val stopped4 = client.stopped
        assert(stopped4.reason == "breakpoint")
        assert(stopped4.threadId == threadId)
        
        client.continue(threadId)
        val stopped5 = client.stopped
        assert(stopped5.reason == "breakpoint")
        assert(stopped5.threadId == threadId)

        client.continue(threadId)
        client.exited
        client.terminated
      } finally {
        server.close()
        client.close()
        IO.delete(tempDir)
      }
    }

    "should support java breakpoints" - {
      val tempDir = IO.createTemporaryDirectory
      val runner = MainDebuggeeRunner.javaBreakpointTest(tempDir)
      val server = new DebugServer(runner, NoopLogger)
      val client = TestDebugClient.connect(server.uri, NoopLogger)
      try {
        server.connect()
        client.initialize()
        client.launch()
        val breakpoints = client.setBreakpoints(runner.source, Array(3, 9, 14, 6))
        assert(breakpoints.forall(_.verified))
        
        client.configurationDone()
        val stopped1 = client.stopped
        val threadId = stopped1.threadId
        assert(stopped1.reason == "breakpoint")
        
        client.continue(threadId)
        val stopped2 = client.stopped
        assert(stopped2.reason == "breakpoint")
        assert(stopped2.threadId == threadId)

        client.continue(threadId)
        val stopped3 = client.stopped
        assert(stopped3.reason == "breakpoint")
        assert(stopped3.threadId == threadId)
        
        client.continue(threadId)
        val stopped4 = client.stopped
        assert(stopped4.reason == "breakpoint")
        assert(stopped4.threadId == threadId)
        
        client.continue(threadId)
        client.exited
        client.terminated
      } finally {
        server.close()
        client.close()
        IO.delete(tempDir)
      }
    }

    "should return stacktrace, scopes and variables when stopped by a breakpoint" - {
      val tempDir = IO.createTemporaryDirectory
      val runner = MainDebuggeeRunner.scalaBreakpointTest(tempDir)
      val server = new DebugServer(runner, NoopLogger)
      val client = TestDebugClient.connect(server.uri, NoopLogger, timeout = DefaultTimeoutMillis millis)
      try {
        server.connect()
        client.initialize()
        client.launch()
        client.setBreakpoints(runner.source, Array(7))
        client.configurationDone()

        val stopped = client.stopped
        val stackTrace = client.stackTrace(stopped.threadId)
        assert(stackTrace.totalFrames == 2)
        
        val topFrame = stackTrace.stackFrames.head
        val scopes = client.scopes(topFrame.id)
        assert(scopes.length == 1)

        val localScope = scopes.head
        assert(localScope.name == "Local")

        val localVars = client.variables(localScope.variablesReference)
        assertMatch(localVars.map(_.name)) {
          case Array("args", "h", "this") => ()
        }

        client.continue(stopped.threadId)
        client.exited
        client.terminated
      } finally {
        server.close()
        client.close()
        IO.delete(tempDir)
      }
    }

    "should cancel debuggee after receiving disconnect request" - {
      val runner = new MockDebuggeeRunner()
      val server = new DebugServer(runner, NoopLogger)
      val client = TestDebugClient.connect(server.uri, NoopLogger)
      
      try {
        val session = server.connect()
        client.initialize()
        client.disconnect(restart = false)
        
        assert(session.currentState == DebugSession.Cancelled)
        Await.result(runner.currentProcess.future(), DefaultTimeoutMillis millis)
      } finally {
        server.close()
        client.close()
      }
    }

    "should accept a second connection when the session disconnects with restart = true" - {
      val runner = new MockDebuggeeRunner()
      val server = new DebugServer(runner, NoopLogger)
      val client1 = TestDebugClient.connect(server.uri, NoopLogger)
      
      try {
        server.start()
        client1.initialize()
        client1.disconnect(restart = true)
        
        val client2 = TestDebugClient.connect(server.uri, NoopLogger)
        try {
          client2.initialize()
        } finally {
          client2.close()
        }

      } finally {
        server.close()
        client1.close()
      }
    }

    "should not accept a second connection when the session disconnects with restart = false" - {
      val runner =  new MockDebuggeeRunner()
      val server = new DebugServer(runner, NoopLogger)
      val client1 = TestDebugClient.connect(server.uri, NoopLogger)
      
      
      try {
        server.start()
        client1.initialize()
        client1.disconnect(restart = false)
        
        var client2: TestDebugClient = null
        try {
          client2 = TestDebugClient.connect(server.uri, NoopLogger)
          client2.initialize()
          assert(false) // it should not accept a second connection
        } catch {
          case _: TimeoutException => ()
        } finally {
          if (client2 != null) client2.close()
        }
        
      } finally {
        server.close()
        client1.close()
      }
    }
  }
}
