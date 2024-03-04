package dotty.tools.dotc

import java.nio.file.Path
import java.util.function.Consumer
import java.{util => ju}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.core.Contexts.Context

class ExpressionCompilerBridge:
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
  ): Boolean =
    val args = Array(
      "-d",
      outDir.toString,
      "-classpath",
      classPath,
      "-Yskip:pureStats"
      // Debugging: Print the tree after phases of the debugger
      // "-Vprint:elimByName,extract-expression,resolve-reflect-eval",
    ) ++ options :+ sourceFile.toString
    val exprCtx =
      ExpressionContext(expressionClassName, line, expression, localVariables.asScala.toSet, pckg, testMode)

    val driver = new Driver:
      protected override def newCompiler(using Context): ExpressionCompiler = ExpressionCompiler(using exprCtx)
    val reporter = ExpressionReporter(error => errorConsumer.accept(error))
    try
      driver.process(args, reporter)
      !reporter.hasErrors
    catch
      case NonFatal(cause) =>
        cause.printStackTrace()
        throw cause
