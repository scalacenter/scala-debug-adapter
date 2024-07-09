package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.testfmk.*

class LocalVariableTests extends DebugTestSuite {
  val scalaVersion = ScalaVersion.`3.1+`

  test("Should set the right expression for array elements") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val array = Array(1, 2, 3)
         |    println("ok")
         |  }
         |}""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(6),
      LocalVariable.inspect("array")(
        _.forall(v => """array\(\d+\)""".r.unapplySeq(v.evaluateName).isDefined)
      )
    )
  }

  test("simple local variables") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val x = 1
         |    println(x)
         |    val y = "2"
         |    println(y)
         |  }
         |}""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(6),
      LocalVariable()(Seq("args", "x", "this")),
      Breakpoint(8),
      LocalVariable()(Seq("args", "x", "y", "this")),
      LocalVariable("y", "CASE_INSENSITIVE_ORDER")(Seq("serialVersionUID"))
    )
  }
}
