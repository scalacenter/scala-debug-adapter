package ch.epfl.scala.debugadapter

import utest._
import ch.epfl.scala.debugadapter.testfmk.*

object MoreScala213StepFilterTests extends DebugTestSuite {
  def tests: Tests = Tests(
    "should match all kinds of Scala 2 types (not valid in Scala 3)" - {
      val source =
        """|package example
           |
           |trait A {
           |  class B
           |}
           |
           |object Main extends A {
           |  class B
           |  def m(b: Main.super[A].B): Main.super[A].B = b
           |  def m(x: Either[Int, X] forSome { type X }): Either[Y, Int] forSome { type Y } = x.swap
           |
           |  def main(args: Array[String]): Unit = {
           |    val b0: super[A].B = new super[A].B
           |    m(b0)
           |    val x = Right(2)
           |    m(x)
           |  }
           |}
           |""".stripMargin
      implicit val debuggee = TestingDebuggee.mainClass(source, "example.Main", ScalaVersion.`2.13`)
      check(Breakpoint(14), StepIn.line(9), Breakpoint(16), StepIn.line(10))
    }
  )
}
