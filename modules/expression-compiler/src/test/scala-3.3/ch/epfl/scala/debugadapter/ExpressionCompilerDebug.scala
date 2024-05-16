package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.testfmk.TestingDebuggee
import dotty.tools.dotc.ExpressionCompilerBridge

import java.nio.file.Files
import scala.collection.mutable.Buffer
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/**
 * This class is used to enter the expression compiler with a debugger
 *  It is not meant to be run in the CI
 */
class ExpressionCompilerDebug extends munit.FunSuite:
  val scalaVersion = ScalaVersion.`3.1+`
  val compiler = new ExpressionCompilerBridge

  override def munitTimeout: Duration = 1.hour

  test("debug test".ignore) {
    val source =
      """|package example
         |
         |object A {
         |  def main(args: Array[String]): Unit = {
         |    println("Hello, World!")
         |  }
         |
         |  val a1 = "a1"
         |  private val a2 = "a2"
         |  private[this] val a3 = "a3"
         |  private[example] val a4 = "a4"
         |
         |  override def toString: String =
         |    a2 + a3
         |
         |  object B {
         |    val b1 = "b1"
         |    private val b2 = "b2"
         |    private[A] val b3 = "b3"
         |    private[example] val b4 = "b4"
         |  }
         |
         |  private object C
         |  private[this] object D
         |  private[example] object E
         |}
         |
         |object F {
         |  val f1 = "f1"
         |  private[example] val f2 = "f2"
         |
         |  object G
         |  private[example] object H
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    evaluate(5, "a2")
  }

  private def evaluate(line: Int, expression: String, localVariables: Set[String] = Set.empty)(using
      debuggee: TestingDebuggee
  ): Unit =
    val out = debuggee.tempDir.resolve("expr-classes")
    if Files.notExists(out) then Files.createDirectory(out)
    val errors = Buffer.empty[String]
    compiler.run(
      out,
      "Expression",
      debuggee.classPathString,
      debuggee.mainModule.scalacOptions.toArray ++ Array("-Xprint:resolve-reflect-eval"),
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
    if errors.nonEmpty then throw new Exception("Evaluation failed")
