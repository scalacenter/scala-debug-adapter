package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.testfmk.*

class Scala212DebugTest extends ScalaDebugTests(ScalaVersion.`2.12`)
class Scala213DebugTest extends ScalaDebugTests(ScalaVersion.`2.13`)
class Scala3DebugTest extends ScalaDebugTests(ScalaVersion.`3.2`) {
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
    implicit val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
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
    implicit val debuggee = TestingDebuggee.mainClass(source, "example.app", scalaVersion)
    check(Breakpoint(4), Breakpoint(10), Breakpoint(6))
  }
}

abstract class ScalaDebugTests(val scalaVersion: ScalaVersion) extends DebugTestSuite {
  test("should support breakpoints in scala sources") {
    val debuggee = TestingDebuggee.scalaBreakpointTest(scalaVersion)
    val server = getDebugServer(debuggee)
    val client = TestingDebugClient.connect(server.uri)
    try {
      server.connect()
      client.initialize()
      client.launch()

      val breakpoints = client.setBreakpoints(debuggee.sourceFiles.head, Array(5, 13, 22, 14, 9))
      assert(breakpoints.size == 5)
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
      val stopped3 = client.stopped()
      assert(stopped3.reason == "breakpoint")
      assert(stopped3.threadId == threadId)

      client.continue(threadId)
      val stopped4 = client.stopped()
      assert(stopped4.reason == "breakpoint")
      assert(stopped4.threadId == threadId)

      client.continue(threadId)
      val stopped5 = client.stopped()
      assert(stopped5.reason == "breakpoint")
      assert(stopped5.threadId == threadId)

      client.continue(threadId)
      client.exited()
      client.terminated()
    } finally {
      server.close()
      client.close()
    }
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
      client.setBreakpoints(debuggee.sourceFiles.head, Array(7))
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
      client.setBreakpoints(debuggee.sourceFiles.head, Array(7))
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

      client.evaluate("1 + 2", topFrame.id)
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
      client.setBreakpoints(debuggee.sourceFiles.head, Array(6, 12))
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
