package ch.epfl.scala.debugadapter

import utest._

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

    "should use tasty-reader" - {
      val scala2Source =
        """|package example
           |
           |trait Msg
           |
           |object Sender {
           |  def send(msg: Msg): Unit = {
           |    println(msg)
           |  }
           |}
           |""".stripMargin

      val scala2Debugee = MainDebuggee.mainClassRunner(scala2Source, "example.Sender", scalaVersion)

      val scala3Source =
        """|package example
           |
           |case class Scala3Msg(msg: String) extends Msg:
           |  override def toString: String = msg
           |
           |object Main:
           |  def main(args: Array[String]): Unit =
           |    Sender.send(Scala3Msg("Hello"))
           |""".stripMargin

      val debuggee = MainDebuggee.mainClassRunner(
        scala3Source,
        "example.Main",
        ScalaVersion.`3.1`,
        Seq.empty,
        Seq(scala2Debugee.mainModule)
      )
      assertInDebuggee(debuggee)(
        Breakpoint(scala2Debugee.sourceFiles.head, 7)(
          Evaluation.success("msg.asInstanceOf[Scala3Msg].msg", "Hello")
        )
      )
    }
  }
}
