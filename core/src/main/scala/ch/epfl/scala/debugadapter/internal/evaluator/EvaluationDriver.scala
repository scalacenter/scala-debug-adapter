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
      code: String,
      line: Int,
      expression: String,
      defNames: Set[String],
      pckg: String,
      errorConsumer: Consumer[String],
      timeout: Duration
  ): Boolean = {
    try {
      compileMethod
        .invoke(
          expressionCompilerInstance,
          expressionDir,
          expressionClassName,
          classPath,
          code,
          Integer.valueOf(line),
          expression,
          defNames.asJava,
          pckg,
          { errorMessage =>
            errorConsumer.accept(errorMessage)
          }: Consumer[String],
          java.lang.Long.valueOf(timeout.toMillis)
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
  ): Try[EvaluationDriver] =
    for {
      clazz <- Try(Class.forName(className, true, classLoader))
      instance <- Try(clazz.getDeclaredConstructor().newInstance())
      method <- Try(clazz.getMethods.find(_.getName == "run").get)
    } yield new EvaluationDriver(instance, method)
}
