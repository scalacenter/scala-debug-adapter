package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.testfmk.TestingDebuggee
import scala.tools.nsc.ExpressionCompilerBridge

import java.nio.file.Files
import scala.collection.mutable.Buffer
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/**
 * This class is used to enter the expression compiler with a debugger
 *  It is not meant to be run in the CI
 */
class ExpressionCompilerDebug extends munit.FunSuite {
  val scalaVersion = ScalaVersion.`2.13`
  val compiler = new ExpressionCompilerBridge

  override def munitTimeout: Duration = 1.hour

  test("report source and position in error, and no colors") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    println("Hello, World!")
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.A", scalaVersion)
    evaluate(5, "\"foo\" + bar", localVariables = Set())
  }

  test("evaluate primitive values") {
    val source =
      """|package example
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    println("Hello, World!")
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.A", scalaVersion)
    evaluate(4, "true", localVariables = Set())
  }

  test("evaluate public and private methods in static object") {
    val source =
      """|package example
         |
         |object A {
         |  def main(args: Array[String]): Unit = {
         |    println("Hello, World!")
         |  }
         |
         |  def a1(str: String) = s"a1: $str"
         |  private def a2(str: String) = s"a2: $str"
         |
         |  private object B {
         |    def b1(str: String) = s"b1: $str"
         |    private[A] def b2(str: String) = s"b2: $str"
         |  }
         |}
         |
         |object C {
         |  def c1(str: String) = s"c1: $str"
         |  private def c2(str: String) = s"c2: $str"
         |}
      """.stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.A", scalaVersion)
    evaluate(5, "a2(\"foo\")", localVariables = Set())
  }

  private def evaluate(line: Int, expression: String, localVariables: Set[String] = Set.empty)(implicit
      debuggee: TestingDebuggee
  ): Unit = {
    val out = debuggee.tempDir.resolve("expr-classes")
    if (Files.notExists(out)) Files.createDirectory(out)
    val errors = Buffer.empty[String]
    compiler.run(
      out,
      "Expression",
      debuggee.classPathString,
      debuggee.mainModule.scalacOptions.toArray,
      debuggee.mainSource,
      line,
      expression,
      localVariables.asJava,
      "example",
      error => {
        println(Console.RED + error + Console.RESET)
        errors += error
      },
      testMode = true
    )
    if (errors.nonEmpty) throw new Exception("Evaluation failed:\n" + errors.mkString("\n"))
  }
}
