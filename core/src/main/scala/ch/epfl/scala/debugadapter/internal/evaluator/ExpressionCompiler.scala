package ch.epfl.scala.debugadapter.internal.evaluator

import java.lang.reflect.Method
import java.nio.file.Path
import java.util.function.Consumer
import scala.collection.JavaConverters._
import scala.util.Try

private[internal] class ExpressionCompiler(
    expressionCompilerInstance: Any,
    compileMethod: Method
) {
  def compile(
      expressionDir: Path,
      expressionClassName: String,
      valuesByNameIdentName: String,
      classPath: String,
      code: String,
      line: Int,
      expression: String,
      defNames: Set[String],
      errorConsumer: Consumer[String]
  ): Boolean = {
    val compiledSuccessfully = Try(
      compileMethod
        .invoke(
          expressionCompilerInstance,
          expressionDir,
          expressionClassName,
          valuesByNameIdentName,
          classPath,
          code,
          Integer.valueOf(line),
          expression,
          defNames.asJava,
          (errorMessage => errorConsumer.accept(errorMessage)): Consumer[String]
        )
    )
      .map(_.asInstanceOf[Boolean])
    compiledSuccessfully.getOrElse(false)
  }
}

private[evaluator] object ExpressionCompiler {
  def apply(evaluationClassLoader: ClassLoader): Option[ExpressionCompiler] =
    for {
      expressionCompilerClass <- Try(
        Class.forName(
          "scala.tools.nsc.ExpressionCompiler",
          true,
          evaluationClassLoader
        )
      ).toOption
      expressionCompilerInstance <- Try(
        expressionCompilerClass.getDeclaredConstructor().newInstance()
      ).toOption
      compileMethod <- expressionCompilerClass.getMethods.find(
        _.getName == "compile"
      )
    } yield new ExpressionCompiler(expressionCompilerInstance, compileMethod)
}
