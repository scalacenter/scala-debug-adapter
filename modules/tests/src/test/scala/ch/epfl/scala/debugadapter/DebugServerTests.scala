package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.internal.DebugSession
import ch.epfl.scala.debugadapter.testfmk.TestingDebugClient

import java.net.{ConnectException, SocketException, SocketTimeoutException}
import scala.concurrent.duration._
import scala.concurrent.Await
import java.util.concurrent.TimeoutException
import ch.epfl.scala.debugadapter.testfmk.*

class DebugServerTests extends DebugTestSuite {
  // the server needs only one thread for delayed responses of the launch and configurationDone requests

  private val connectionFailedMessages = Array(
    "(Connection refused)",
    "(Connection Failed)",
    "(connect failed)",
    "Connection reset by peer"
  )

  private def noGracePeriod: DebugConfig = defaultConfig.copy(gracePeriod = Duration.Zero)

  test("should prevent connection when closed") {
    val debuggee = new MockDebuggee()
    val server = getDebugServer(debuggee, noGracePeriod)
    server.close()
    try {
      TestingDebugClient.connect(server.uri)
      assert(false) // connect was supposed to fail
    } catch {
      case _: SocketTimeoutException => ()
      case _: ConnectException => ()
      case e: SocketException =>
        val msg = e.getMessage
        assert(connectionFailedMessages.exists(msg.endsWith))
    }
  }

  test("should start session when client connects") {
    val debuggee = new MockDebuggee()
    val server = getDebugServer(debuggee, noGracePeriod)
    val client = TestingDebugClient.connect(server.uri)
    try {
      val session = server.connect()
      assert(
        clue(session.currentState) match {
          case DebugSession.Started(_) => true
          case _ => false
        }
      )
    } finally {
      server.close()
      client.close()
    }
  }

  test("should stop session when closed") {
    val debuggee = new MockDebuggee()
    val server = getDebugServer(debuggee, noGracePeriod)
    val client = TestingDebugClient.connect(server.uri)
    try {
      val session = server.connect()
      server.close()
      assert(session.currentState == DebugSession.Stopped)
    } finally {
      server.close()
      client.close()
    }
  }

  test("should initialize only one connection") {
    val debuggee = new MockDebuggee()
    val server = getDebugServer(debuggee, noGracePeriod)
    val client1 = TestingDebugClient.connect(server.uri)
    try {
      server.connect()
      client1.initialize()

      var client2: TestingDebugClient = null
      try {
        client2 = TestingDebugClient.connect(server.uri)
        client2.initialize(1.second)
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

  test("should not launch if the debuggee has not started a jvm") {
    val debuggee = new MockDebuggee()
    val server = getDebugServer(debuggee, noGracePeriod)
    val client = TestingDebugClient.connect(server.uri)
    try {
      server.connect()
      client.initialize()
      assert(!client.launch().success)
    } finally {
      server.close()
      client.close()
    }
  }

  test("should set debug address when debuggee starts the jvm") {
    val server = getDebugServer(TestingDebuggee.sleep)
    val client = TestingDebugClient.connect(server.uri)
    try {
      val session = server.connect()
      Await.result(session.getDebugeeAddress, 2.seconds)
    } finally {
      server.close()
      client.close()
    }
  }

  test("should respond failure to launch request if debugee throws an exception") {
    val debuggee = new MockDebuggee()
    val config = defaultConfig.copy(gracePeriod = Duration.Zero)
    val server = getDebugServer(debuggee, config)
    val client = TestingDebugClient.connect(server.uri)
    try {
      server.connect()
      debuggee.currentProcess.stopped.failure(new Exception())
      client.initialize()
      assert(!client.launch().success)
    } finally {
      server.close()
      client.close()
    }
  }

  test("should launch and send initialized event if the debuggee has started a jvm") {
    val server = getDebugServer(TestingDebuggee.sleep)
    val client = TestingDebugClient.connect(server.uri)
    try {
      server.connect()
      client.initialize()
      assert(client.launch().success)
      client.initialized()
      client.configurationDone()
    } finally {
      server.close()
      client.close()
    }
  }

  test("should send terminated events when closed") {
    val server = getDebugServer(TestingDebuggee.sleep)
    val client = TestingDebugClient.connect(server.uri)
    try {
      server.connect()
      client.initialize()
      client.launch()
      client.configurationDone()
      // give some time for the debuggee to terminate gracefully
      server.close()
      client.terminated()
    } finally {
      server.close() // in case test fails
      client.close()
    }
  }

  test("should send output event when debuggee prints to stdout") {
    implicit val debuggee: TestingDebuggee = TestingDebuggee.helloWorld
    check(Outputed("Hello, World!"))
  }

  test("should send exited and terminated events when debuggee exits successfully") {
    val server = getDebugServer(TestingDebuggee.helloWorld)
    val client = TestingDebugClient.connect(server.uri)
    try {
      server.connect()
      client.initialize()
      client.launch()
      client.configurationDone()
      // the debuggee should exits immediately
      val exitedEvent = client.exited()
      assert(exitedEvent.exitCode == 0)

      client.terminated()
    } finally {
      server.close()
      client.close()
    }
  }

  test("should start debuggee with duplicate entries") {
    val debuggee = TestingDebuggee.helloWorld
    val withDuplicate = debuggee.copy(dependencies = debuggee.dependencies ++ debuggee.dependencies)
    val server = getDebugServer(withDuplicate)
    val client = TestingDebugClient.connect(server.uri)
    try {
      server.connect()
      client.initialize()
      client.launch()
      client.configurationDone()
      client.exited()
      client.terminated()
    } finally {
      server.close()
      client.close()
    }
  }

  test("should send exited and terminated events when debuggee exits in error") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    sys.exit(1)
         |  }
         |}
         |
         |""".stripMargin
    val debuggee = TestingDebuggee.mainClass(source, "example.Main", ScalaVersion.`2.12`)
    val server = getDebugServer(debuggee)
    val client = TestingDebugClient.connect(server.uri)
    try {
      server.connect()
      client.initialize()
      client.launch()
      client.configurationDone()
      // the debuggee should exits immediately
      val exitedEvent = client.exited()

      // surprisingly, the java debug implementation always set exitCode to 0
      // https://github.com/microsoft/java-debug/blob/211c47aabec6d47d8393ec39b6fdf0cbfcd8dbb0/com.microsoft.java.debug.core/src/main/java/com/microsoft/java/debug/core/adapter/handler/ConfigurationDoneRequestHandler.java#L84
      assert(exitedEvent.exitCode == 0)

      client.terminated()
    } finally {
      server.close()
      client.close()
    }
  }

  test("should support breakpoints in java classes") {
    val source =
      """|package example;
         |
         |class Main {
         |  public static void main(String[] args) {
         |    System.out.println("Breakpoint in main method");
         |    Main test = new Main();
         |    test.greet();
         |    System.out.println("Finished all breakpoints");
         |  }
         |
         |  public Main() {
         |    System.out.println("Breakpoint in constructor");
         |  }
         |
         |  public void greet() {
         |    System.out.println("Breakpoint in method");
         |  }
         |}
         |
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.fromJavaSource(source, "example.Main", ScalaVersion.`2.12`)
    check(Breakpoint(5), Breakpoint(12), Breakpoint(16), Breakpoint(8))
  }

  test("should cancel debuggee after receiving disconnect request") {
    val debuggee = new MockDebuggee()
    val server = getDebugServer(debuggee)
    val client = TestingDebugClient.connect(server.uri)

    try {
      val session = server.connect()
      client.initialize()
      client.disconnect(restart = false)

      Await.result(debuggee.currentProcess.future, 2.seconds)
    } finally {
      server.close()
      client.close()
    }
  }

  test("should accept a second connection when the session disconnects with restart = true") {
    val debuggee = new MockDebuggee()
    val handler = startDebugServer(debuggee, gracePeriod = Duration.Zero)
    val client1 = TestingDebugClient.connect(handler.uri)

    try {
      client1.initialize()
      client1.disconnect(restart = true)

      val client2 = TestingDebugClient.connect(handler.uri)
      try {
        client2.initialize()
      } finally {
        client2.close()
      }

    } finally {
      client1.close()
    }
  }

  test("should not accept a second connection when the session disconnects with restart = false") {
    val debuggee = new MockDebuggee()
    val handler = startDebugServer(debuggee, gracePeriod = Duration.Zero)
    val client1 = TestingDebugClient.connect(handler.uri)

    try {
      client1.initialize()
      client1.disconnect(restart = false)

      var client2: TestingDebugClient = null
      try {
        client2 = TestingDebugClient.connect(handler.uri)
        client2.initialize(1.second)
        assert(false) // it should not accept a second connection
      } catch {
        case _: TimeoutException => ()
        case _: ConnectException => ()
        case _: SocketTimeoutException => ()
        case e: SocketException =>
          val msg = e.getMessage
          assert(connectionFailedMessages.exists(msg.endsWith))
      } finally {
        if (client2 != null) client2.close()
      }

    } finally {
      client1.close()
    }
  }
}
