package scala.tools.nsc

import java.nio.file.{Files, Path}
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.{Global => Compiler}

object ExpressionCompiler {
  def apply(classPath: String): ExpressionCompiler = {
    val dir = Files.createTempDirectory("expr-eval")
    val settings = new Settings
    settings.classpath.value = classPath
    settings.outputDirs.setSingleOutput(dir.toString)
    val reporter = new StoreReporter
    val pc = new Compiler(settings, reporter)
    new ExpressionCompiler(pc, reporter, dir)
  }
}

class ExpressionCompiler(private val compiler: Compiler, private val reporter: StoreReporter, val dir: Path) {
  private val compilerRun = new compiler.Run()

  def compile(expression: String): Unit = {
    val expressionClass =
      s"""class Expression {
         |  // taking `String` instead of `Any` causes "Class java.lang.String not loaded" exception
         |  def evaluate(names: Array[Any], values: Array[Any]) = {
         |    val valuesByName = names.map(_.asInstanceOf[String]).zip(values).toMap
         |    val a = valuesByName("a").asInstanceOf[Int]
         |    val b = valuesByName("b").asInstanceOf[Int]
         |    val c = valuesByName("c").asInstanceOf[String]
         |    $expression
         |  }
         |}
         |""".stripMargin
    val source: BatchSourceFile = new BatchSourceFile(
      "<expression>",
      expressionClass
    )
    compilerRun.compileSources(List(source))
    reporter.infos.foreach(println)
  }
}
