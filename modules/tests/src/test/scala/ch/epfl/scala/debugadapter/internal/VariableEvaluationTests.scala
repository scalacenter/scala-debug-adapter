package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.testfmk.DebugTestSuite
import ch.epfl.scala.debugadapter.testfmk.TestingDebuggee
import ch.epfl.scala.debugadapter.testfmk.Breakpoint
import ch.epfl.scala.debugadapter.testfmk.Watch

object VariableEvaluationEnvironment {
  val array =
    """|package example
       |
       |object Main {
       |  def main(args: Array[String]): Unit = {
       |    val array = Array(1, 2, 3)
       |    println("ok")
       |  }
       |}""".stripMargin
}

abstract class VariableEvaluationTests(val scalaVersion: ScalaVersion) extends DebugTestSuite {
  lazy val array = TestingDebuggee.mainClass(VariableEvaluationEnvironment.array, "example.Main", scalaVersion)

  test("Should set the right expression for array elements") {
    implicit val debuggee = array
    val regexp = """array\((\d+)\)""".r
    check(
      Breakpoint(6),
      Watch.success("array") {
        _.forall { v =>
          v.evaluateName match {
            case regexp(_) => true
            case _ => false
          }
        }
      }
    )
  }
}

case class Scala212VariableEvaluationTests() extends VariableEvaluationTests(ScalaVersion.`2.12`)
case class Scala213VariableEvaluationTests() extends VariableEvaluationTests(ScalaVersion.`2.13`)
case class Scala3VariableEvaluationTests() extends VariableEvaluationTests(ScalaVersion.`3.1+`)
