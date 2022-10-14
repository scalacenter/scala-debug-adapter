package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.testfmk.*

class Scala213BreakpointConditionTests extends BreakpointConditionTests(ScalaVersion.`2.13`)
class Scala32BreakpointConditionTests extends BreakpointConditionTests(ScalaVersion.`3.2`)

abstract class BreakpointConditionTests(scalaVersion: ScalaVersion) extends DebugTestSuite {
  test("evaluate simple breakpoint") {
    val source =
      """|package example
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val x = 42
         |    println(x)
         |  }
         |}
         |""".stripMargin
    implicit val debugggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(5, "x == 42"), Evaluation.success("x", 42))
  }

  test("evaluate breakpoint in lambda") {
    val source =
      """|package example
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    List(1, 2, 3).map { i => 
         |      val msg = i.toString
         |      println(msg)
         |    }
         |    println("Hello, World!")
         |  }
         |}
         |""".stripMargin
    implicit val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(5, "i == 2"), Evaluation.success("i", 2), Breakpoint(8))
  }
}
