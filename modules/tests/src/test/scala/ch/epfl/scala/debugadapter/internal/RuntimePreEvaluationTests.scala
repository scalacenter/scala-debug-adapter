package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.testfmk.*
import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.DebugConfig

class Scala212RuntimePreEvaluationTests extends RuntimePreEvaluationTests(ScalaVersion.`2.12`)
class Scala213RuntimePreEvaluationTests extends RuntimePreEvaluationTests(ScalaVersion.`2.13`)
class Scala3RuntimePreEvaluationTests extends RuntimePreEvaluationTests(ScalaVersion.`3.1+`)

abstract class RuntimePreEvaluationTests(val scalaVersion: ScalaVersion) extends DebugTestSuite {

  protected override def defaultConfig: DebugConfig =
    super.defaultConfig.copy(evaluationMode = DebugConfig.RuntimeEvaluationOnly)

  test("Should pre-evaluate local variables to get a more precise type") {
    val source =
      """|package example
         |
         |trait Foo {
         |  def foo: Int
         |}
         |case class Bar(foo: Int) extends Foo {
         |  val bar: Int = foo
         |}
         |case class Baz(foo: Int) extends Foo {
         |  val baz: Int = foo
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val bar: Foo = Bar(1)
         |    val baz: Foo = Baz(2)
         |    println("ok")
         |  }
         |}
       """.stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(17),
      Evaluation.success("bar.bar", 1),
      Evaluation.success("baz.baz", 2)
    )
  }

  test("Should pre-evaluate modules to get a more precise type") {
    val source =
      """|package example
         |
         |sealed trait Foo
         |case object Bar extends Foo {
         |  val bar: Int = 1
         |}
         |case object Baz extends Foo {
         |  val baz: Int = 2
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val bar: Foo = Bar
         |    val baz: Foo = Baz
         |    println("ok")
         |  }
         |}
       """.stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(15),
      Evaluation.success("bar.bar", 1),
      Evaluation.success("baz.baz", 2)
    )
  }

  test("Should pre-evaluate modules nested in classes to get a more precise type") {
    val source =
      """|package example
         |
         |case class Foo() {
         |  sealed trait InnerFoo
         |  case object Bar extends InnerFoo {
         |    val bar: Int = 1
         |  }
         |  case object Baz extends InnerFoo {
         |    val baz: Int = 2
         |  }
         |  
         |  val innerBar: InnerFoo = Bar
         |  val innerBaz: InnerFoo = Baz
         |}
         |
         |object Foo {
         |  sealed trait NestedFoo
         |  case object BarO extends NestedFoo {
         |    val bar: Int = 1
         |  }
         |  case object BazO extends NestedFoo {
         |    val baz: Int = 2
         |  }
         |  val nestedBar: NestedFoo = BarO
         |  val nestedBaz: NestedFoo = BazO
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val foo: Foo = Foo()
         |    println("ok")
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(31),
      Evaluation.success("foo.innerBar.bar", 1),
      Evaluation.success("foo.innerBaz.baz", 2),
      Evaluation.success("Foo.nestedBar.bar", 1),
      Evaluation.success("Foo.nestedBaz.baz", 2)
    )
  }
}
