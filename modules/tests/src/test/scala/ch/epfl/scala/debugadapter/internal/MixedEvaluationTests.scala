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
       |    implicit val x: Int = 42
       |    val foo = Foo()
       |    println(foo.wrongEval)
       |  }
       |}
       |
       |trait SuperFoo {
       |  def wrongEval: Boolean = true
       |}
       |
       |case class Foo() extends SuperFoo {
       |  def wrongEval(implicit x: Int): Boolean = false
       |}
       |""".stripMargin
}

class Scala212MixedEvaluationTests extends MixedEvaluationTests(ScalaVersion.`2.12`)
class Scala213MixedEvaluationTests extends MixedEvaluationTests(ScalaVersion.`2.13`)
class Scala31MixedEvaluationTests extends MixedEvaluationTests(ScalaVersion.`3.1+`)

abstract class MixedEvaluationTests(val scalaVersion: ScalaVersion) extends DebugTestSuite {
  lazy val localVar =
    TestingDebuggee.mainClass(MixedEvaluationTestsSource.source, "example.Main", scalaVersion)

  test(
    "Should produce wrong output at runtime when overloads require compiler to be resolved, but should succeed with a mixed evaluation (fallback to compiler)"
  ) {
    implicit val debuggee = localVar
    check(defaultConfig.copy(evaluationMode = DebugConfig.RuntimeEvaluationOnly))(
      Breakpoint(7),
      Evaluation.success("foo.wrongEval", true),
      Evaluation.success("foo.wrongEval(42)", false)
    )
    check(defaultConfig.copy(evaluationMode = DebugConfig.MixedEvaluation))(
      Breakpoint(7),
      Evaluation.success("foo.wrongEval", false)
    )
  }

}
