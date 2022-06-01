package scala.tools.nsc

import java.nio.file.Path
import java.util.function.Consumer
import java.{util => ju}
import scala.collection.JavaConverters._
import scala.tools.nsc.reporters.StoreReporter
import scala.util.control.NonFatal

final class EvaluationBridge {
  def run(
      expressionDir: Path,
      expressionClassName: String,
      classPath: String,
      sourceFile: Path,
      line: Int,
      expression: String,
      localVariables: ju.Set[String],
      pckg: String,
      errorConsumer: Consumer[String],
      timeoutMillis: Long
  ): Boolean = {
    val settings = new Settings
    // Debugging: Print the tree after each phases of the debugger
    // settings.Xprint.value = List("insert-expression", "typer", "generate-expression")
    settings.classpath.value = classPath
    settings.outputDirs.setSingleOutput(expressionDir.toString)
    val reporter = new StoreReporter
    val global = new EvaluationGlobal(
      settings,
      reporter,
      line,
      expression,
      localVariables.asScala.toSet,
      expressionClassName
    )

    try {
      val run = new global.Run()
      run.compile(List(sourceFile.toString))

      val error = reporter.infos.find(_.severity == reporter.ERROR).map(_.msg)
      error.foreach(errorConsumer.accept)
      error.isEmpty
    } catch {
      case NonFatal(t) =>
        t.printStackTrace()
        errorConsumer.accept(t.getMessage())
        false
    }
  }
}
