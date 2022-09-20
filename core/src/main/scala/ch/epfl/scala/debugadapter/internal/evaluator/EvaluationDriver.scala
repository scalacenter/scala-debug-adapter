package ch.epfl.scala.debugadapter.internal.evaluator

import java.lang.reflect.Method
import java.lang.reflect.InvocationTargetException
import java.nio.file.Path
import java.util.function.Consumer
import scala.collection.JavaConverters._
import scala.util.Try
import scala.concurrent.duration._

private[internal] class EvaluationDriver(
    expressionCompilerInstance: Any,
    compileMethod: Method
) {
  def run(
      expressionDir: Path,
      expressionClassName: String,
      classPath: String,
      sourceFile: Path,
      line: Int,
      expression: String,
      defNames: Set[String],
      pckg: String,
      reportError: String => Unit,
      timeout: Duration,
      testMode: Boolean
  ): Boolean = {
    try {
      compileMethod
        .invoke(
          expressionCompilerInstance,
          expressionDir,
          expressionClassName,
          classPath,
          sourceFile,
          line: java.lang.Integer,
          expression,
          defNames.asJava,
          pckg,
          { error => reportError(error) }: Consumer[String],
          timeout.toMillis: java.lang.Long,
          testMode: java.lang.Boolean
        )
        .asInstanceOf[Boolean]
    } catch {
      case cause: InvocationTargetException =>
        throw cause.getCause()
    }
  }
}

private[internal] object EvaluationDriver {
  def apply(classLoader: ClassLoader): Option[EvaluationDriver] = {
    loadBridge(classLoader, "scala.tools.nsc.EvaluationBridge")
      .orElse(loadBridge(classLoader, "dotty.tools.dotc.EvaluationBridge"))
      .toOption
  }

  private def loadBridge(
      classLoader: ClassLoader,
      className: String
  ): Try[EvaluationDriver] = {
    for {
      clazz <- Try(Class.forName(className, true, classLoader))
      instance <- Try(clazz.getDeclaredConstructor().newInstance())
      method <- Try(clazz.getMethods.find(_.getName == "run").get)
    } yield new EvaluationDriver(instance, method)
  }
}
