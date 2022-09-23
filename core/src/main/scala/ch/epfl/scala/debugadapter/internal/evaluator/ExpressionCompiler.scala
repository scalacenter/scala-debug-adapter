package ch.epfl.scala.debugadapter.internal.evaluator

import java.lang.reflect.Method
import java.lang.reflect.InvocationTargetException
import java.nio.file.Path
import java.util.function.Consumer
import scala.collection.JavaConverters._
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import scala.collection.mutable.Buffer
import ch.epfl.scala.debugadapter.ScalaVersion

private[debugadapter] class ExpressionCompiler(
    instance: Any,
    compileMethod: Method,
    val scalaVersion: ScalaVersion,
    scalacOptions: Seq[String],
    classPath: String
) {
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
      if (res) Success(()) else Failure(new CompilationFailed(errors.toSeq))
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
      else "dotty.tools.dotc.ExpressionCompilerBridge"

    try {
      val clazz = Class.forName(className, true, classLoader)
      val instance = clazz.getDeclaredConstructor().newInstance()
      val method = clazz.getMethods.find(_.getName == "run").get
      Success(new ExpressionCompiler(instance, method, scalaVersion, scalacOptions, classPath))
    } catch {
      case cause: Throwable => Failure(cause)
    }
  }
}
