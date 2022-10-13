package ch.epfl.scala.debugadapter

import utest.*
import ch.epfl.scala.debugadapter.testfmk.*

object MoreScala3DebugTests extends DebugTestSuite {
  def tests: Tests = Tests {
    "should support breakpoints in scala 3 with brace-less syntax" - {
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
      implicit val debuggee = TestingDebuggee.mainClass(source, "example.Main", ScalaVersion.`3.2`)
      check(Breakpoint(5), Breakpoint(11), Breakpoint(7))
    }

    "should support breakpoints in scala 3 with @main" - {
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
      implicit val debuggee = TestingDebuggee.mainClass(source, "example.app", ScalaVersion.`3.2`)
      check(Breakpoint(4), Breakpoint(10), Breakpoint(6))
    }
  }
}
