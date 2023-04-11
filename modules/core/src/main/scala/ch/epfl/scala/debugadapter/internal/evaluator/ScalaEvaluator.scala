package ch.epfl.scala.debugadapter.internal.evaluator

import ch.epfl.scala.debugadapter.ClassEntry
import ch.epfl.scala.debugadapter.Logger
import com.sun.jdi._

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import scala.util.Try

private[internal] class ScalaEvaluator(
    entry: ClassEntry,
    compiler: ExpressionCompiler,
    logger: Logger,
    testMode: Boolean
) {
  def evaluate(expression: CompiledExpression, frame: JdiFrame): Try[Value] = {
    val CompiledExpression(classDir, className) = expression
    evaluate(classDir, className, frame)
  }

  def compile(sourceContent: String, expression: String, frame: JdiFrame): Try[CompiledExpression] = {
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
        (names, values) <- extractValuesAndNames(frame, classLoader)
        localNames = names.map(_.stringValue).toSet
        _ <- compiler
          .compile(outDir, expressionClassName, sourceFile, line, expression, localNames, packageName, testMode)
          .toSafe
      } yield CompiledExpression(outDir, expressionFqcn)
    compiledExpression.getResult
  }

  private def evaluate(classDir: Path, className: String, frame: JdiFrame): Try[Value] = {
    val evaluatedValue = for {
      classLoader <- frame.classLoader()
      (names, values) <- extractValuesAndNames(frame, classLoader)
      namesArray <- classLoader.createArray("java.lang.String", names)
      valuesArray <- classLoader.createArray("java.lang.Object", values)
      args = List(namesArray, valuesArray)
      expressionInstance <- createExpressionInstance(classLoader, classDir, className, args)
      evaluatedValue <- evaluateExpression(expressionInstance)
      _ <- updateVariables(valuesArray, frame)
      unboxedValue <- evaluatedValue.unboxIfPrimitive
    } yield unboxedValue.value
    evaluatedValue.getResult
  }

  private def evaluateExpression(expressionInstance: JdiObject): Safe[JdiValue] = {
    expressionInstance
      .invoke("evaluate", List())
      .recover {
        // if evaluation throws an exception, we return that exception as the result
        case MethodInvocationFailed(msg, exception) => exception
      }
  }

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
  private def extractValuesAndNames(
      frameRef: JdiFrame,
      classLoader: JdiClassLoader
  ): Safe[(Seq[JdiString], Seq[JdiValue])] = {
    def extractVariablesFromFrame(): Safe[(Seq[JdiString], Seq[JdiValue])] = {
      val localVariables = frameRef.variablesAndValues().map { case (variable, value) => (variable.name, value) }
      // Exclude the this object if there already is a local $this variable
      // The Scala compiler uses `$this` in the extension methods of AnyVal classes
      val thisObject = frameRef.thisObject.filter(_ => !localVariables.contains("$this")).map("$this".->)
      (localVariables ++ thisObject)
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
    // Only useful in Scala 2
    def extractFields(thisObject: JdiObject): Safe[(Seq[JdiString], Seq[JdiValue])] = {
      val fields = thisObject.fields
      val names = fields.map(_._1).map(classLoader.mirrorOf).traverse
      val values = fields.map(_._2).map(value => classLoader.boxIfPrimitive(value)).traverse
      Safe.join(names, values)
    }

    for {
      (variableNames, variableValues) <- extractVariablesFromFrame()
      // Currently we only need to load the fields for the Scala 2
      // expression evaluator.
      // It is dangerous because local values can shadow fields
      // TODO: adapt Scala 2 expression compiler
      (fieldNames, fieldValues) <- frameRef.thisObject
        .filter(_ => compiler.scalaVersion.isScala2)
        .map(extractFields)
        .getOrElse(Safe((Nil, Nil)))
      // If `this` is a value class then the breakpoint must be in
      // a generated method of its companion object which takes
      // the erased value`$this` as argument.
    } yield {
      val names = variableNames ++ fieldNames
      val values = variableValues ++ fieldValues
      (names, values)
    }
  }

  private def updateVariables(variableArray: JdiArray, frame: JdiFrame): Safe[Unit] = {
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
