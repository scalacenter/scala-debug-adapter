package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.testfmk.*
import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.DebugConfig

object MixedEvaluationTestsSource {
  val source =
    """|package example
       |
       |object Main {
       |  def main(args: Array[String]): Unit = {
       |    m1(true)
       |    m2(() => false)
       |  }
       |
       |  def m1(x: => Boolean): Boolean =
       |    x
       |
       |  def m2(x: () => Boolean): Boolean =
       |    x()
       |}
       |""".stripMargin

}

class Scala212MixedEvaluationTests extends MixedEvaluationTests(ScalaVersion.`2.12`)
class Scala213MixedEvaluationTests extends MixedEvaluationTests(ScalaVersion.`2.13`)
class Scala31MixedEvaluationTests extends MixedEvaluationTests(ScalaVersion.`3.1+`)

abstract class MixedEvaluationTests(val scalaVersion: ScalaVersion) extends DebugTestSuite {
  lazy val localVar =
    TestingDebuggee.mainClass(MixedEvaluationTestsSource.source, "example.Main", scalaVersion)

  protected override def defaultConfig: DebugConfig =
    super.defaultConfig.copy(evaluationMode = DebugConfig.RuntimeEvaluationOnly)

  test("Should not compute by-name param or Function0 expression") {
    implicit val debuggee: TestingDebuggee =
      TestingDebuggee.mainClass(MixedEvaluationTestsSource.source, "example.Main", scalaVersion)
    // the evaluator refuses to bypass the compiler because it does not know
    // if x is a Function0 or a by-name param
    check(Breakpoint(10), Evaluation.failed("x"), Breakpoint(13), Evaluation.failed("x"))
  }
}
