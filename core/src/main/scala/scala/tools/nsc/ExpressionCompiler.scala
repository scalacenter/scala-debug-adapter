package scala.tools.nsc

import java.nio.file.{Files, Path}
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.reporters.StoreReporter

object ExpressionCompiler {
  val ExpressionId: String = java.util.UUID.randomUUID.toString.replace("-", "")
  val ExpressionClassName: String = s"Expression$ExpressionId"
  val ValuesByNameIdentName: String = s"valuesByName$ExpressionId"

  def apply(classPath: String, line: Int, expression: String, valOrDefDefNames: Set[String]): ExpressionCompiler = {
    val dir = Files.createTempDirectory("expr-eval")
    val settings = new Settings
    settings.classpath.value = classPath
    settings.outputDirs.setSingleOutput(dir.toString)
    val reporter = new StoreReporter
    val global = new EvalGlobal(settings, reporter, line, expression, valOrDefDefNames)
    new ExpressionCompiler(global, reporter, dir)
  }

  def expressionClassFqcn(fqcn: String): String = {
    (fqcn.split("\\.").dropRight(1) :+ ExpressionClassName).mkString(".")
  }
}

class ExpressionCompiler(val global: EvalGlobal, val reporter: StoreReporter, val dir: Path) {
  private val compilerRun = new global.Run()

  def compile(code: String, report: String => Unit): Option[Unit] = {
    val source = new BatchSourceFile("<source>", code)
    compilerRun.compileSources(List(source))
    val error = reporter.infos.find(_.severity == reporter.ERROR).map(_.msg)
    error.foreach(report)
    if (error.isDefined) None
    else Some(())
  }
}
