package scala.tools.nsc.interactive

import java.nio.file.{Files, Path}
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.StoreReporter

object ExpressionCompiler {
  // Do we need this?
  trait Dummy

  def apply(classPath: String): ExpressionCompiler = {
    val dir = Files.createTempDirectory("expr-eval")
    val settings = new Settings
    settings.embeddedDefaults[Dummy]
    settings.classpath.value = "/home/tdudzik/.sbt/boot/scala-2.12.12/lib/scala-library.jar"
    settings.outputDirs.setSingleOutput(dir.toString)
    val reporter = new StoreReporter
    val pc = new Global(settings, reporter)
    new ExpressionCompiler(pc, reporter, dir)
  }
}

class ExpressionCompiler(private val pc: Global, private val reporter: StoreReporter, val dir: Path) {
  private val pcRun = new pc.Run()

  def compile(expression: String): Unit = {
    val expressionObject =
      s"""class Expression {
         |  def evaluate() = {
         |    $expression
         |  }
         |}
         |""".stripMargin
    val source = new BatchSourceFile(
      "<virtual>",
      expressionObject
    )
    pc.ask(() => pcRun.compileSources(List(source)))
    reporter.infos.foreach(println)
  }
}
