package ch.epfl.scala.debugadapter

import utest._

object MoreScala30EvaluationTests extends MoreScala3EvaluationTests(ScalaVersion.`3.0`)
object MoreScala31EvaluationTests extends MoreScala3EvaluationTests(ScalaVersion.`3.1`)

abstract class MoreScala3EvaluationTests(scalaVersion: ScalaVersion)
    extends ScalaEvaluationSuite(scalaVersion) {

  override def tests: Tests = Tests {
    "evaluate shadowed variable properly" - {
      val source =
        """|object EvaluateTest {
           |  def main(args: Array[String]): Unit = {
           |    val foo = "foo"
           |    {
           |      val foo = "bar"
           |      println(foo)
           |    }
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(6)(ExpressionEvaluation.success("foo", "bar"))
      )
    }

    "evaluate variable shadowed in other scope properly" - {
      val source =
        """|object EvaluateTest {
           |  def main(args: Array[String]): Unit = {
           |    val foo = "foo"
           |    {
           |      val foo = "bar"
           |      println(foo)
           |    }
           |    println(foo)
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(8)(ExpressionEvaluation.success("foo", "foo"))
      )
    }
  }
}
