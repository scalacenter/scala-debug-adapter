package ch.epfl.scala.debugadapter

import utest._

object MoreScala212EvaluationTests extends MoreScala2EvaluationTests(ScalaVersion.`2.12`)
object MoreScala213EvaluationTests extends MoreScala2EvaluationTests(ScalaVersion.`2.13`)

abstract class MoreScala2EvaluationTests(scalaVersion: ScalaVersion) extends ScalaEvaluationSuite(scalaVersion) {

  override def tests: Tests = Tests {
    "should use -Xsource:3" - {
      val source =
        """|package example
           |
           |import scala.{*, given}
           |open class A
           |
           |object Main {
           |  type Foo = String & Int
           |
           |  def main(args: Array[String]): Unit = {
           |    println(m(Seq("a", "b")*))
           |  }
           |
           |  def m(xs: String*): String = xs.mkString(", ")
           |}
           |""".stripMargin
      assertInMainClass(source, "example.Main", Seq("-Xsource:3"))(
        Breakpoint(10)(Evaluation.success("""m(Seq("a", "b")*)""", "a, b"))
      )
    }
  }
}
