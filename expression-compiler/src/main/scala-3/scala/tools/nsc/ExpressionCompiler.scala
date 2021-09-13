package scala.tools.nsc

import java.nio.file.Path
import java.util.function.Consumer
import java.{util => ju}

final class ExpressionCompiler {
  def compile(
      expressionDir: Path,
      expressionClassName: String,
      valuesByNameIdentName: String,
      classPath: String,
      code: String,
      line: Int,
      expression: String,
      defNames: ju.Set[String],
      errorConsumer: Consumer[String],
      timeoutMillis: Long
  ): Boolean = {
    errorConsumer.accept("The Scala debugger does not yet support Scala 3")
    false
  }
}
