package ch.epfl.scala.debugadapter.internal.evaluator

import ch.epfl.scala.debugadapter.Logger
import com.sun.jdi._

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import scala.jdk.CollectionConverters.*
import scala.util.Try
import ch.epfl.scala.debugadapter.ClassEntry

private[internal] class ScalaEvaluator(
    entry: ClassEntry,
    compiler: ExpressionCompiler,
    logger: Logger,
    testMode: Boolean
) {
  def evaluate(expression: CompiledExpression, frame: FrameReference): Try[Value] = {
    val CompiledExpression(classDir, className) = expression
    evaluate(classDir, className, frame)
  }

  def compile(sourceContent: String, expression: String, frame: FrameReference): Try[CompiledExpression] = {
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
    val classLoader = findClassLoader(frame)
    val compiledExpression =
      for {
        (names, values) <- extractValuesAndNames(frame, classLoader)
        localNames = names.map(_.value()).toSet
        _ <- Safe(
          compiler.compile(outDir, expressionClassName, sourceFile, line, expression, localNames, packageName, testMode)
        )
      } yield CompiledExpression(outDir, expressionFqcn)
    compiledExpression.getResult
  }

  private def evaluate(classDir: Path, className: String, frame: FrameReference): Try[Value] = {
    val classLoader = findClassLoader(frame)
    val evaluatedValue = for {
      (names, values) <- extractValuesAndNames(frame, classLoader)
      namesArray <- JdiArray("java.lang.String", names.size, classLoader)
      valuesArray <- JdiArray("java.lang.Object", values.size, classLoader)
      _ = namesArray.setValues(names)
      _ = valuesArray.setValues(values)
      args = List(namesArray.reference, valuesArray.reference)
      expressionInstance <- createExpressionInstance(classLoader, classDir, className, args)
      evaluatedValue <- evaluateExpression(expressionInstance)
      _ <- updateVariables(valuesArray, frame)
      unboxedValue <- unboxIfPrimitive(evaluatedValue, frame.thread)
    } yield unboxedValue
    evaluatedValue.getResult
  }

  private def findClassLoader(frame: FrameReference): JdiClassLoader = {
    val scalaLibClassLoader =
      for {
        scalaLibClass <- frame.thread.virtualMachine.allClasses.asScala
          .find(c => c.name.startsWith("scala.runtime"))
        classLoader <- Option(scalaLibClass.classLoader)
      } yield classLoader

    val classLoader = Option(frame.current().location.method.declaringType.classLoader)
      .orElse(scalaLibClassLoader)
      .getOrElse(throw new Exception("Cannot find the classloader of the Scala library"))
    JdiClassLoader(classLoader, frame.thread)
  }

  private def evaluateExpression(expressionInstance: JdiObject): Safe[Value] = {
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
      args: List[ObjectReference]
  ): Safe[JdiObject] = {
    val expressionClassPath = classDir.toUri.toString
    for {
      classPathValue <- classLoader.mirrorOf(expressionClassPath)
      urlClass <- classLoader
        .loadClass("java.net.URL")
      url <- urlClass.newInstance(List(classPathValue))
      urls <- JdiArray("java.net.URL", 1, classLoader)
      _ = urls.setValue(0, url.reference)
      urlClassLoader <- classLoader
        .loadClass("java.net.URLClassLoader")
        .flatMap(_.newInstance(List(urls.reference)))
        .map(_.reference.asInstanceOf[ClassLoaderReference])
        .map(JdiClassLoader(_, classLoader.thread))
      expressionClass <- urlClassLoader.loadClass(className)
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
      frameRef: FrameReference,
      classLoader: JdiClassLoader
  ): Safe[(Seq[StringReference], Seq[Value])] = {
    val frame = frameRef.current()
    val thisObjectOpt = Option(frame.thisObject) // this object can be null
    def extractVariablesFromFrame(): Safe[(Seq[StringReference], Seq[Value])] = {
      val localVariables = frame.visibleVariables().asScala.toSeq.map(v => v.name -> frame.getValue(v))
      val thisObject = thisObjectOpt.filter(_ => !localVariables.exists(_._1 == "$this")).map("$this".->)
      (localVariables ++ thisObject)
        .map { case (name, value) =>
          for {
            name <- classLoader.mirrorOf(name)
            value <- boxIfPrimitive(value, classLoader)
          } yield (name, value)
        }
        .traverse
        .map(xs => (xs.map(_._1), xs.map(_._2)))
    }
    // Only useful in Scala 2
    def extractFields(thisObject: ObjectReference): Safe[(Seq[StringReference], Seq[Value])] = {
      val fields = thisObject.referenceType.fields.asScala.toList
      val fieldNames = fields.map(_.name).map(classLoader.mirrorOf).traverse
      val fieldValues = fields
        .map(field => thisObject.getValue(field))
        .map(value => boxIfPrimitive(value, classLoader))
        .traverse
      Safe.join(fieldNames, fieldValues)
    }

    for {
      (variableNames, variableValues) <- extractVariablesFromFrame()
      // Currently we only need to load the fields for the Scala 2
      // expression evaluator.
      // It is dangerous because local values can shadow fields
      // TODO: adapt Scala 2 expression compiler
      (fieldNames, fieldValues) <- thisObjectOpt
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

  private def updateVariables(variableArray: JdiArray, frame: FrameReference): Safe[Unit] = {
    def localVariables(): List[LocalVariable] =
      // we must get a new StackFrame object after each invocation
      frame.current().visibleVariables().asScala.toList

    val unboxedValues = localVariables()
      .zip(variableArray.getValues)
      .map { case (variable, value) =>
        if (isPrimitive(variable)) unboxIfPrimitive(value, frame.thread)
        else Safe(value)
      }
      .traverse

    for (values <- unboxedValues)
      yield {
        for ((variable, value) <- localVariables().zip(values)) {
          frame.current().setValue(variable, value)
        }
      }
  }

  private def boxIfPrimitive(value: Value, classLoader: JdiClassLoader): Safe[Value] = {
    val thread = classLoader.thread
    value match {
      case value: BooleanValue =>
        JdiPrimitive.box(value.value(), classLoader, thread)
      case value: CharValue =>
        JdiPrimitive.box(value.value(), classLoader, thread)
      case value: DoubleValue =>
        JdiPrimitive.box(value.value(), classLoader, thread)
      case value: FloatValue =>
        JdiPrimitive.box(value.value(), classLoader, thread)
      case value: IntegerValue =>
        JdiPrimitive.box(value.value(), classLoader, thread)
      case value: LongValue =>
        JdiPrimitive.box(value.value(), classLoader, thread)
      case value: ShortValue =>
        JdiPrimitive.box(value.value(), classLoader, thread)
      case value => Safe(value)
    }
  }

  private val unboxMethods = Map(
    "java.lang.Boolean" -> "booleanValue",
    "java.lang.Byte" -> "byteValue",
    "java.lang.Character" -> "charValue",
    "java.lang.Double" -> "doubleValue",
    "java.lang.Float" -> "floatValue",
    "java.lang.Integer" -> "intValue",
    "java.lang.Long" -> "longValue",
    "java.lang.Short" -> "shortValue"
  )

  private def unboxIfPrimitive(value: Value, thread: ThreadReference): Safe[Value] = {
    value match {
      case ref: ObjectReference =>
        val typeName = ref.referenceType.name
        unboxMethods
          .get(typeName)
          .map(methodName => new JdiObject(ref, thread).invoke(methodName, Nil))
          .getOrElse(Safe(value))
      case _ => Safe(value)
    }
  }

  private def isPrimitive(variable: LocalVariable): Boolean =
    variable.`type`().isInstanceOf[PrimitiveType]
}
