package scala.tools.nsc

import java.nio.file.{Files, Path}
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.reporters.StoreReporter

object ExpressionCompiler {
  // TODO: make unique
  val ExpressionClassName = "Expression"

  def apply(classPath: String, line: Int, valOrDefDefNames: Set[String]): ExpressionCompiler = {
    val dir = Files.createTempDirectory("expr-eval")
    val settings = new Settings
    settings.classpath.value = classPath
    settings.outputDirs.setSingleOutput(dir.toString)
    val reporter = new StoreReporter
    val global = new EvalGlobal(settings, reporter, line, valOrDefDefNames)
    new ExpressionCompiler(global, reporter, dir)
  }
}

class ExpressionCompiler(val global: EvalGlobal, val reporter: StoreReporter, val dir: Path) {
  private val expressionSource =
    s"""
      |class ${ExpressionCompiler.ExpressionClassName} {
      |  def evaluate(names: Array[Any], values: Array[Any]) = {
      |    val valuesByName = names.map(_.asInstanceOf[String]).zip(values).toMap
      |    valuesByName
      |    ()
      |  }
      |}
      |""".stripMargin

  private val compilerRun = new global.Run()

  def compile(code: String, expression: String, report: String => Unit): Option[Unit] = {
    val codeWithExpression = code + "\n" + expressionSource
    val lines = codeWithExpression.split("\n")
    val newCode = (lines.take(global.line - 1) ++ Seq(expression) ++ lines.drop(global.line - 1)).mkString("\n")
    val source = new BatchSourceFile(
      "<source>",
      newCode
    )
    compilerRun.compileSources(List(source))
    val error = reporter.infos.find(_.severity == reporter.ERROR).map(_.msg)
    error.foreach(report)
    if (error.isDefined) None
    else Some(())
  }
}
