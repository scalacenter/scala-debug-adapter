package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.testfmk.TestingDebuggee
import ch.epfl.scala.debugadapter.ScalaVersion
import dotty.tools.dotc.ExpressionCompilerBridge
import java.nio.file.Files
import scala.collection.mutable.Buffer
import java.{util => ju}
import scala.jdk.CollectionConverters.*
import scala.concurrent.duration.*

/**
 * This class is used to enter the expression compiler with a debugger
 *  It is not meant to be run in the CI
 */
class ExpressionCompilerDebug extends munit.FunSuite:
  val scalaVersion = ScalaVersion.`3.1+`
  val compiler = new ExpressionCompilerBridge

  override def munitTimeout: Duration = 1.hour

  test("tuple extractor") {
    val source =
      """|package example
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val tuple = (1, 2)
         |    val (x, y) = tuple
         |    println("ok")
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    evaluate(5, "tuple._1", localVariables = Set("tuple"))
  }

  def evaluate(line: Int, expression: String, localVariables: Set[String] = Set.empty)(using
      debuggee: TestingDebuggee
  ): Unit =
    val out = debuggee.tempDir.resolve("expr-classes")
    if Files.notExists(out) then Files.createDirectory(out)
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
    if errors.nonEmpty then throw new Exception("Evaluation failed")
