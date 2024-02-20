package ch.epfl.scala.debugadapter
import ch.epfl.scala.debugadapter.testfmk.DebugTestSuite
import ch.epfl.scala.debugadapter.testfmk.TestingDebuggee
import ch.epfl.scala.debugadapter.testfmk.Breakpoint
import ch.epfl.scala.debugadapter.testfmk.Custom
import ch.epfl.scala.debugadapter.testfmk.StepIn
import ch.epfl.scala.debugadapter.testfmk.Evaluation
import ch.epfl.scala.debugadapter.testfmk.HotCodeReplace
import ch.epfl.scala.debugadapter.testfmk.Outputed

class HotCodeReplaceTests extends DebugTestSuite {
  protected override def defaultConfig: DebugConfig =
    super.defaultConfig.copy(evaluationMode = DebugConfig.RuntimeEvaluationOnly)

  test("introduce new class") {
    val `Main.scala` =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    A.m
         |    println("success")
         |  }
         |}
         |
         |""".stripMargin
    val `A.scala` =
      """|package example
         |
         |object A {
         |  def m =
         |    println("A")
         |}
         |""".stripMargin
    val `A.scala.new` =
      """|package example
         |
         |object A {
         |  def m =
         |    B.m
         |}
         |
         |object B {
         |  def m = {
         |    println("B")
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(
      Seq("Main.scala" -> `Main.scala`, "A.scala" -> `A.scala`),
      "example.Main",
      ScalaVersion.`3.3`
    )
    check(
      Breakpoint(5),
      Evaluation.failed("B", "B is not a local variable"),
      Breakpoint(debuggee.getSourceFile("A.scala"), 5),
      Custom {
        debuggee.compileScala("A.scala" -> `A.scala.new`)
        debuggee.classesToUpdate.onNext(Seq("example.A$", "example.B$"))
      },
      HotCodeReplace("A.m: Unit"),
      StepIn.method("B.m: Unit"),
      Breakpoint(6),
      Evaluation.success("B.m")(res => res == "<void value>")
    )
  }

  test("pop two frames in the same class") {
    val `Main.scala` =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = A.m1
         |}
         |
         |""".stripMargin
    val `A.scala` =
      """|package example
         |
         |object A {
         |  def m1: Unit =
         |    A.m2
         |
         |  def m2: Unit =
         |    println("m2")
         |}
         |""".stripMargin
    val `A.scala.new` =
      """|package example
         |
         |object A {
         |  def m1: Unit =
         |    println("m1")
         |
         |  def m2: Unit = ??? // JVM cannot delete a method
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(
      Seq("Main.scala" -> `Main.scala`, "A.scala" -> `A.scala`),
      "example.Main",
      ScalaVersion.`3.3`
    )
    check(
      Breakpoint(debuggee.getSourceFile("A.scala"), 8, "A.m2: Unit"),
      Custom {
        debuggee.compileScala("A.scala" -> `A.scala.new`)
        debuggee.classesToUpdate.onNext(Seq("example.A$"))
      },
      HotCodeReplace("A.m1: Unit"),
      Outputed("m1")
    )
  }
}
