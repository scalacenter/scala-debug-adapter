package ch.epfl.scala.debugadapter

import utest._

object MoreScala213StepFilterTests
    extends StepFilterSuite(ScalaVersion.`2.13`) {
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
      assertInMainClass(source, "example.Main")(
        Breakpoint(14)(StepInto.line(9)),
        Breakpoint(16)(StepInto.line(10))
      )
    }
  )
}
