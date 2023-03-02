package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.testfmk.*
import scala.concurrent.Await
import scala.concurrent.duration._

class Scala212DebugTest extends ScalaDebugTests(ScalaVersion.`2.12`)
class Scala213DebugTest extends ScalaDebugTests(ScalaVersion.`2.13`)
class Scala3DebugTest extends ScalaDebugTests(ScalaVersion.`3.1+`) {
  test("should support breakpoints in scala 3 with brace-less syntax") {
    val source =
      """|package example
         |
         |object Main:
         |  def main(args: Array[String]): Unit =
         |    println("Breakpoint in main method")
         |    new Hello().greet()
         |    println("Finished all breakpoints")
         |
         |  class Hello():
         |    def greet(): Unit =
         |      println("Breakpoint in hello class")
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(5), Breakpoint(11), Breakpoint(7))
  }
  test("should support breakpoints in scala 3 with @main") {
    val source =
      """|package example
         |
         |@main def app: Unit =
         |  println("Breakpoint in main method")
         |  new Hello().greet()
         |  println("Finished all breakpoints")
         |
         |class Hello():
         |  def greet(): Unit =
         |    println("Breakpoint in hello class")
         |
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.app", scalaVersion)
    check(Breakpoint(4), Breakpoint(10), Breakpoint(6))
  }
}

abstract class ScalaDebugTests(val scalaVersion: ScalaVersion) extends DebugTestSuite {
  test("should not crash when sources aren't present") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    foo("world")
         |  }
         |
         |  def foo(name: String): Unit = {
         |    println(name)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee =
      TestingDebuggee.mainClassWithoutJDKSources(source, "example.Main", scalaVersion)
    check(Breakpoint(5), StepIn.line(9))
  }
  test("should support breakpoints in scala sources") {
    implicit val debuggee = TestingDebuggee.scalaBreakpointTest(scalaVersion)
    check(Breakpoint(5), Breakpoint(13), Breakpoint(14), Breakpoint(22), Breakpoint(9))
  }

  test("supports breakpoints in top level class and object") {
    val source =
      """|object Main {
         |  def main(args: Array[String]): Unit = {
         |    println("main method")
         |    (new A).m()
         |  }
         |}
         |class A {
         |  def m(): Unit = {
         |    println("method m in class A")
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "Main", scalaVersion)
    check(Breakpoint(3), Breakpoint(9))
  }

  test("should support breakpoints in fully qualified classes") {
    val debuggee = TestingDebuggee.scalaBreakpointTest(scalaVersion)
    val server = getDebugServer(debuggee)
    val client = TestingDebugClient.connect(server.uri)
    try {
      server.connect()
      client.initialize()
      client.launch()

      val breakpoints = client.setBreakpointsInClass(debuggee.mainClass, Array(5, 9))
      assert(breakpoints.size == 2)
      assert(breakpoints.forall(_.verified))

      client.configurationDone()
      val stopped1 = client.stopped()
      val threadId = stopped1.threadId
      assert(stopped1.reason == "breakpoint")

      client.continue(threadId)
      val stopped2 = client.stopped()
      assert(stopped2.reason == "breakpoint")
      assert(stopped2.threadId == threadId)

      client.continue(threadId)
      client.exited()
      client.terminated()
    } finally {
      server.close()
      client.close()
    }
  }

  test("should return stacktrace, scopes and variables when stopped by a breakpoint") {
    val debuggee = TestingDebuggee.scalaBreakpointTest(scalaVersion)
    val server = getDebugServer(debuggee)
    val client = TestingDebugClient.connect(server.uri)
    try {
      server.connect()
      client.initialize()
      client.launch()
      client.setBreakpoints(debuggee.sourceFiles.head, Seq(7))
      client.configurationDone()

      val stopped = client.stopped()
      val stackTrace = client.stackTrace(stopped.threadId)
      assert(stackTrace.totalFrames == 2)

      val topFrame = stackTrace.stackFrames.head
      val scopes = client.scopes(topFrame.id)
      assert(scopes.length == 1)

      val localScope = scopes.head
      assert(localScope.name == "Local")

      val localVars = client.variables(localScope.variablesReference)
      assertEquals(localVars.map(_.name).toSeq, Seq("args", "h", "this"))
      assertEquals(localVars(1).value.split(" ").last, "\"hello\"")

      client.continue(stopped.threadId)
      client.exited()
      client.terminated()
    } finally {
      server.close()
      client.close()
    }
  }

  test("should return variables after expression evaluation") {
    val debuggee = TestingDebuggee.scalaBreakpointTest(scalaVersion)
    val server = getDebugServer(debuggee)
    val client = TestingDebugClient.connect(server.uri)
    try {
      server.connect()
      client.initialize()
      client.launch()
      client.setBreakpoints(debuggee.sourceFiles.head, Seq(7))
      client.configurationDone()

      val stopped = client.stopped()
      val stackTrace = client.stackTrace(stopped.threadId)
      assert(stackTrace.totalFrames == 2)

      val topFrame = stackTrace.stackFrames.head
      val scopes = client.scopes(topFrame.id)
      assert(scopes.length == 1)

      val localScope = scopes.head
      assert(localScope.name == "Local")

      val localVars = client.variables(localScope.variablesReference)
      assertEquals(localVars.map(_.name).toSeq, Seq("args", "h", "this"))

      val evalResponse = Await.result(client.evaluate("1 + 2", topFrame.id), 16.second)
      assertEquals(evalResponse, Right("3"))
      val localVarsAfterEvaluation = client.variables(localScope.variablesReference)
      assertEquals(localVarsAfterEvaluation.map(_.name).toSeq, Seq("args", "h", "this"))

      client.continue(stopped.threadId)
      client.exited()
      client.terminated()
    } finally {
      server.close()
      client.close()
    }
  }

  test("should invoke custom toString even if there is a breakpoint inside") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a = new A()
         |    println("exiting")
         |  }
         |}
         |
         |class A {
         |  override def toString(): String = {
         |    "B"
         |  }
         |}
         |""".stripMargin
    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val server = getDebugServer(debuggee)
    val client = TestingDebugClient.connect(server.uri)
    try {
      server.connect()
      client.initialize()
      client.launch()
      client.setBreakpoints(debuggee.sourceFiles.head, Seq(6, 12))
      client.configurationDone()

      val stopped = client.stopped()
      val stackTrace = client.stackTrace(stopped.threadId)
      val topFrame = stackTrace.stackFrames.head
      val scopes = client.scopes(topFrame.id)
      assert(scopes.length == 1)

      val localScope = scopes.head
      assert(localScope.name == "Local")

      val localVars = client.variables(localScope.variablesReference)
      assert(localVars.map(_.value).exists(_.contains("\"B\"")))

      client.continue(stopped.threadId)
      client.exited()
      client.terminated()
    } finally {
      server.close()
      client.close()
    }
  }
}
