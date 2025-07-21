package ch.epfl.scala.debugadapter.internal.evaluator

import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.internal.Errors

import java.lang.reflect.InvocationTargetException
import java.lang.reflect.Method
import java.nio.file.Path
import java.util.function.Consumer
import scala.collection.mutable.Buffer
import scala.jdk.CollectionConverters.*
import scala.util.Failure
import scala.util.Success
import scala.util.Try

private[debugadapter] trait ExpressionCompiler {
  def compile(
      outDir: Path,
      expressionClassName: String,
      sourceFile: Path,
      line: Int,
      expression: String,
      localNames: Set[String],
      pckg: String,
      testMode: Boolean
  ): Try[Unit]
}

private[debugadapter] class ExpressionCompilerPre37(
    instance: Any,
    compileMethod: Method,
    val scalaVersion: ScalaVersion,
    scalacOptions: Seq[String],
    classPath: String
) extends ExpressionCompiler {
  def compile(
      outDir: Path,
      expressionClassName: String,
      sourceFile: Path,
      line: Int,
      expression: String,
      localNames: Set[String],
      pckg: String,
      testMode: Boolean
  ): Try[Unit] = {
    try {
      val errors = Buffer.empty[String]
      val res = compileMethod
        .invoke(
          instance,
          outDir,
          expressionClassName,
          classPath,
          scalacOptions.toArray,
          sourceFile,
          line: java.lang.Integer,
          expression,
          localNames.asJava,
          pckg,
          { error => errors += error }: Consumer[String],
          testMode: java.lang.Boolean
        )
        .asInstanceOf[Boolean]
      if (res) Success(()) else Failure(Errors.compilationFailure(errors.toSeq))
    } catch {
      case cause: InvocationTargetException => Failure(cause.getCause())
    }
  }
}

private[debugadapter] class ExpressionCompilerPost37(
    instance: Any,
    compileMethod: Method,
    config: (
        /*packageName:*/ String, /*outputClassName:*/ String, /*breakpointLine:*/ Int, /*expression:*/ String,
        /*localVariables:*/ java.util.Set[String], /*errorReporter:*/ Consumer[String], /*testMode:*/ Boolean
    ) => Object,
    val scalaVersion: ScalaVersion,
    scalacOptions: Seq[String],
    classPath: String
) extends ExpressionCompiler {
  def compile(
      outDir: Path,
      expressionClassName: String,
      sourceFile: Path,
      line: Int,
      expression: String,
      localNames: Set[String],
      pckg: String,
      testMode: Boolean
  ): Try[Unit] = {
    try {
      val errors = Buffer.empty[String]
      val configInstance = config(
        pckg,
        expressionClassName,
        line,
        expression,
        localNames.asJava,
        { error => errors += error }: Consumer[String],
        testMode
      )
      val res = compileMethod
        .invoke(
          instance,
          outDir,
          classPath,
          scalacOptions.toArray,
          sourceFile,
          configInstance
        )
        .asInstanceOf[Boolean]
      if (res) Success(()) else Failure(Errors.compilationFailure(errors.toSeq))
    } catch {
      case cause: InvocationTargetException => Failure(cause.getCause())
    }
  }
}

private[debugadapter] object ExpressionCompiler {
  def apply(
      scalaVersion: ScalaVersion,
      scalacOptions: Seq[String],
      classPath: String,
      classLoader: ClassLoader
  ): Try[ExpressionCompiler] = {
    val className =
      if (scalaVersion.isScala2) "scala.tools.nsc.ExpressionCompilerBridge"
      else if (scalaVersion.isScala3 && scalaVersion.minor >= 7) {
        "dotty.tools.debug.ExpressionCompilerBridge"
      } else {
        "dotty.tools.dotc.ExpressionCompilerBridge"
      }

    try {
      val clazz = Class.forName(className, true, classLoader)
      val instance = clazz.getDeclaredConstructor().newInstance()
      val method = clazz.getMethods.find(_.getName == "run").get
      if (scalaVersion.isScala3 && scalaVersion.minor >= 7) {
        Success(
          new ExpressionCompilerPost37(
            instance,
            method,
            expressionCompilerConfig(classLoader),
            scalaVersion,
            scalacOptions,
            classPath
          )
        )
      } else {
        Success(new ExpressionCompilerPre37(instance, method, scalaVersion, scalacOptions, classPath))
      }
    } catch {
      case cause: Throwable => Failure(cause)
    }
  }

  private def expressionCompilerConfig(classLoader: ClassLoader)(
      packageName: String,
      outputClassName: String,
      breakpointLine: Int,
      expression: String,
      localVariables: java.util.Set[String],
      errorReporter: Consumer[String],
      testMode: Boolean
  ): Object = {
    val clazz = Class.forName("dotty.tools.debug.ExpressionCompilerConfig", true, classLoader)
    val instance = clazz.getDeclaredConstructor().newInstance()
    val withPackageName = clazz.getMethod("withPackageName", classOf[String]).invoke(instance, packageName)
    val withOutputClassName =
      clazz.getMethod("withOutputClassName", classOf[String]).invoke(withPackageName, outputClassName)
    val withBreakpointLine = clazz
      .getMethod("withBreakpointLine", classOf[Int])
      .invoke(withOutputClassName, Integer.valueOf(breakpointLine))
    val withExpression = clazz.getMethod("withExpression", classOf[String]).invoke(withBreakpointLine, expression)
    val withLocalVariables =
      clazz.getMethod("withLocalVariables", classOf[java.util.Set[String]]).invoke(withExpression, localVariables)
    val withErrorReporter =
      clazz.getMethod("withErrorReporter", classOf[Consumer[String]]).invoke(withLocalVariables, errorReporter)
    withErrorReporter.asInstanceOf[Object]
  }
}
