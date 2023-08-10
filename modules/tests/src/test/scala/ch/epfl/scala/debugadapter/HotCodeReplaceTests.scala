package ch.epfl.scala.debugadapter
import ch.epfl.scala.debugadapter.testfmk.DebugTestSuite
import ch.epfl.scala.debugadapter.testfmk.TestingDebuggee
import ch.epfl.scala.debugadapter.testfmk.Breakpoint
import ch.epfl.scala.debugadapter.testfmk.Outputed
import ch.epfl.scala.debugadapter.testfmk.Custom
import ch.epfl.scala.debugadapter.testfmk.RedefineClasses
import ch.epfl.scala.debugadapter.testfmk.StepIn
import ch.epfl.scala.debugadapter.testfmk.Evaluation

class HotCodeReplaceTests extends DebugTestSuite {
  protected override def defaultConfig: DebugConfig =
    super.defaultConfig.copy(evaluationMode = DebugConfig.RuntimeEvaluationOnly)

  test("hot test") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit =
         |    new A().m()
         |}
         |
         |class A {
         |  def m() = {
         |    println("A")
         |    println("B")
         |  }
         |}
         |""".stripMargin
    val newSource =
      """|package example
         |
         |class A {
         |  def m() = {
         |    println("C")
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", ScalaVersion.`3.1+`)
    check(
      Outputed("A"),
      Breakpoint(11),
      Custom {
        debuggee.compileScala(newSource, "hotCodeReloadTest")
        debuggee.classesToUpdate.onNext(Seq("example.A"))
      },
      RedefineClasses(),
      Outputed("C")
    )
  }

  test("new hot test") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit ={
         |    Foo.foo
         |    println("ok")
         |  }
         |}
         |
         |""".stripMargin
    val fooSource =
      """|package example
         |
         |object Foo {
         |  def foo =
         |    println("A")
         |}
         |""".stripMargin
    val newFooSource =
      """|package example
         |
         |object Foo {
         |  def foo =
         |    A.m
         |}
         |
         |object A {
         |  def m = {
         |    println("C")
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(
      Seq("Main.scala" -> source, "Foo.scala" -> fooSource),
      "example.Main",
      ScalaVersion.`3.1+`
    )
    check(defaultConfig.copy(testMode = false))(
      Breakpoint(5),
      Evaluation.failed("A"),
      Breakpoint(debuggee.getPathOf("Foo.scala"), 5),
      Custom {
        debuggee.compileScala(newFooSource, "Foo.scala")
        debuggee.classesToUpdate.onNext(Seq("example.Foo$", "example.A$"))
      },
      RedefineClasses(),
      StepIn.method("A.m: Unit"),
      Breakpoint(6),
      Evaluation.success("A.m")(res => res == "<void value>")
    )
  }
}
