package scala.tools.nsc

import java.nio.file.Path
import java.util.function.Consumer
import java.{util => ju}
import scala.jdk.CollectionConverters._
import scala.tools.nsc.evaluation.ExpressionGlobal
import scala.util.control.NonFatal

final class ExpressionCompilerBridge {
  def run(
      outDir: Path,
      expressionClassName: String,
      classPath: String,
      options: Array[String],
      sourceFile: Path,
      line: Int,
      expression: String,
      localVariables: ju.Set[String],
      pckg: String,
      errorConsumer: Consumer[String],
      testMode: Boolean
  ): Boolean = {
    val args = List(
      "-d",
      outDir.toString,
      "-classpath",
      classPath
      // Debugging: Print the tree after phases of the debugger
      // "-Xprint:insert-expression,extract-expression,resolve-reflect-eval",
      // "-Vdebug"
    ) ++ options :+ sourceFile.toString

    val command = new CompilerCommand(args, errorConsumer.accept(_))
    val reporter = new ExpressionReporter(errorConsumer.accept, command.settings)
    val global = new ExpressionGlobal(
      command.settings,
      reporter,
      expressionClassName,
      line,
      expression,
      localVariables.asScala.toSet,
      pckg,
      testMode
    )

    try {
      val run = new global.Run()
      run.compile(List(sourceFile.toString))
      !reporter.hasErrors
    } catch {
      case NonFatal(t) =>
        t.printStackTrace()
        errorConsumer.accept(t.getMessage())
        false
    }
  }
}
