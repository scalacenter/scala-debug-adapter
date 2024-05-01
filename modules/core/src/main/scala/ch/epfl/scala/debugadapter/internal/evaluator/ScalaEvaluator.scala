package ch.epfl.scala.debugadapter.internal.evaluator

import ch.epfl.scala.debugadapter.Logger
import com.sun.jdi._

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import scala.util.Try

private[internal] class ScalaEvaluator(
    sourceContent: String,
    frame: JdiFrame,
    compiler: ExpressionCompiler,
    logger: Logger,
    testMode: Boolean
) {
  def evaluate(expression: CompiledExpression): Try[Value] =
    evaluate(expression.classDir, expression.className)

  def compile(expression: String): Try[CompiledExpression] = {
    logger.debug(s"Compiling expression '$expression'")
    val location = frame.current().location
    val line = location.lineNumber
    val fqcn = location.declaringType.name
    val className = fqcn.split('.').last
    val packageName = if (className == fqcn) "" else fqcn.stripSuffix(s".$className")

    val randomId = java.util.UUID.randomUUID.toString.replace("-", "")
    val outDir = Files.createTempDirectory(s"scala-debug-adapter-$randomId")
    val expressionClassName = s"Expression$randomId"

    val fileName = location.sourcePath.split('/').flatMap(_.split('\\')).last
    val sourceFile = Files.createFile(outDir.resolve(fileName))
    Files.write(sourceFile, sourceContent.getBytes(StandardCharsets.UTF_8))

    val expressionFqcn = if (packageName.isEmpty) expressionClassName else s"$packageName.$expressionClassName"
    val compiledExpression =
      for {
        classLoader <- frame.classLoader()
        (names, values) <- extractLocalValuesAndNames(classLoader)
        localNames = names.map(_.stringValue).toSet
        _ <- compiler
          .compile(outDir, expressionClassName, sourceFile, line, expression, localNames, packageName, testMode)
          .toSafe
      } yield CompiledExpression(outDir, expressionFqcn)
    compiledExpression.getResult
  }

  private def evaluate(classDir: Path, className: String): Try[Value] = {
    val evaluatedValue = for {
      classLoader <- frame.classLoader()
      (names, values) <- extractLocalValuesAndNames(classLoader)
      namesArray <- classLoader.createArray("java.lang.String", names)
      valuesArray <- classLoader.createArray("java.lang.Object", values)
      thisObject = frame.thisObject.getOrElse(JdiNull)
      args = List(thisObject, namesArray, valuesArray)
      expressionInstance <- createExpressionInstance(classLoader, classDir, className, args)
      evaluatedValue <- evaluateExpression(expressionInstance)
      _ <- updateVariables(valuesArray)
      unboxedValue <- evaluatedValue.unboxIfPrimitive
    } yield unboxedValue.value
    evaluatedValue.getResult
  }

  private def evaluateExpression(expressionInstance: JdiObject): Safe[JdiValue] =
    expressionInstance.invoke("evaluate", List())

  /**
   * In order to load the previously compiled Expression class, we need to
   * first load and instantiate URL with expressionClassPath
   * and then URLClassLoader with the url created before.
   */
  private def createExpressionInstance(
      classLoader: JdiClassLoader,
      classDir: Path,
      className: String,
      args: List[JdiObject]
  ): Safe[JdiObject] = {
    val expressionClassPath = classDir.toUri.toString
    for {
      expressionClassLoader <- classLoader.createChildLoader(classDir)
      expressionClass <- expressionClassLoader.loadClass(className)
      expressionInstance <- expressionClass.newInstance(args)
    } yield expressionInstance
  }

  /**
   * Extract all values and their corresponding names which are visible in current scope.
   * Values consist of:
   * - variables from stack frame
   * - fields from this object
   * @return Tuple of extracted names and values
   */
  private def extractLocalValuesAndNames(classLoader: JdiClassLoader): Safe[(Seq[JdiString], Seq[JdiValue])] = {
    val localVariables = frame.variablesAndValues().map { case (variable, value) => (variable.name, value) }
    localVariables
      .map { case (name, value) =>
        for {
          name <- classLoader.mirrorOf(name)
          value <- classLoader.boxIfPrimitive(value)
        } yield (name, value)
      }
      .toSeq
      .traverse
      .map(xs => (xs.map(_._1), xs.map(_._2)))
  }

  private def updateVariables(variableArray: JdiArray): Safe[Unit] = {
    frame
      .variables()
      .zip(variableArray.getValues)
      .map {
        case (variable, value) if variable.`type`.isInstanceOf[PrimitiveType] => value.unboxIfPrimitive
        case (_, value) => Safe(value)
      }
      .traverse
      .map { values =>
        frame
          .variables()
          .zip(values)
          .foreach { case (variable, value) => frame.setVariable(variable, value) }
      }
  }
}
