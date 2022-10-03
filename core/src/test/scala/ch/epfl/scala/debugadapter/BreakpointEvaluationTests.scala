package ch.epfl.scala.debugadapter

import utest._

object Scala213BreakpointEvaluationTests extends BreakpointEvaluationTests(ScalaVersion.`2.13`)
object Scala32BreakpointEvaluationTests extends BreakpointEvaluationTests(ScalaVersion.`3.2`)

abstract class BreakpointEvaluationTests(scalaVersion: ScalaVersion) extends ScalaEvaluationSuite(scalaVersion) {

  def tests: Tests = Tests {
    "evaluate simple breakpoint" - {
      val source =
        """|package example
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    val x = 42
           |    println(x)
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "example.Main")(
        Breakpoint(5, "x == 42")(Evaluation.success("x", 42))
      )
    }

    "evaluate breakpoint in lambda" - {
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
      assertInMainClass(source, "example.Main")(
        Breakpoint(5, "i == 2")(Evaluation.success("i", 2)),
        Breakpoint(8)()
      )
    }
  }
}
