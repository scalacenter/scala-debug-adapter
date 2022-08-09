package ch.epfl.scala.debugadapter.internal.evaluator

import com.microsoft.java.debug.core.adapter.ISourceLookUpProvider
import com.sun.jdi._

import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.util.Try
import java.nio.charset.StandardCharsets
import ch.epfl.scala.debugadapter.Logger

private[internal] class ExpressionEvaluator(
    scalaVersion: String,
    classPath: Seq[Path],
    sourceLookUpProvider: ISourceLookUpProvider,
    driver: EvaluationDriver,
    logger: Logger
) {
  private val classPathString =
    classPath.mkString(File.pathSeparator)

  def evaluate(
      expression: String,
      thread: ThreadReference,
      depth: Int
  ): Try[Value] = {
    logger.debug(s"Evaluating '$expression'")
    val location = thread.frame(depth).location
    val sourcePath = location.sourcePath
    val breakpointLine = location.lineNumber
    val fqcn = location.declaringType.name
    val className = fqcn.split('.').last
    val pckg =
      if (className == fqcn) ""
      else fqcn.stripSuffix(s".$className")

    val uri = sourceLookUpProvider.getSourceFileURI(fqcn, sourcePath)
    val sourceContent = sourceLookUpProvider.getSourceContents(uri)

    val randomId = java.util.UUID.randomUUID.toString.replace("-", "")
    val expressionDir =
      Files.createTempDirectory(s"scala-debug-adapter-$randomId")
    val expressionClassName = s"Expression$randomId"

    val fileName = sourcePath.split('/').flatMap(_.split('\\')).last
    val sourceFile = Files.createFile(expressionDir.resolve(fileName))
    Files.write(sourceFile, sourceContent.getBytes(StandardCharsets.UTF_8))

    val expressionFqcn =
      (fqcn.split("\\.").dropRight(1) :+ expressionClassName).mkString(".")
    var errors = Seq.empty[String]
    val classLoader = findClassLoader(thread)
    val evaluatedValue = for {
      // must be called before any invocation otherwise
      // it throws an InvalidStackFrameException
      (names, values) <- extractValuesAndNames(thread.frame(depth), classLoader)
      compiled = driver
        .run(
          expressionDir,
          expressionClassName,
          classPathString,
          sourceFile,
          breakpointLine,
          expression,
          names.map(_.value()).toSet,
          pckg,
          error => errors :+= error,
          5.seconds
        )
      _ = {
        if (!compiled) {
          logger.debug(s"Failed compilation:\n${errors.mkString("\n")}")
          throw new ExpressionCompilationFailed(errors)
        }
      }
      // if everything went smooth we can load our expression class
      namesArray <-
        JdiArray("java.lang.String", names.size, classLoader)
      valuesArray <-
        JdiArray("java.lang.Object", values.size, classLoader)
      _ = namesArray.setValues(names)
      _ = valuesArray.setValues(values)
      args = List(namesArray.reference, valuesArray.reference)
      expressionInstance <-
        createExpressionInstance(
          classLoader,
          expressionDir,
          expressionFqcn,
          args
        )
      evaluatedValue <- evaluateExpression(expressionInstance)
      _ <- updateVariables(valuesArray, thread, depth)
      unboxedValue <- unboxIfPrimitive(evaluatedValue, thread)
    } yield unboxedValue
    evaluatedValue.getResult
  }

  private def evaluateExpression(
      expressionInstance: JdiObject
  ): Safe[Value] = {
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
      expressionDir: Path,
      expressionFqcn: String,
      args: List[ObjectReference]
  ): Safe[JdiObject] = {
    val expressionClassPath = expressionDir.toUri.toString
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
      expressionClass <- urlClassLoader.loadClass(expressionFqcn)
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
      frame: StackFrame,
      classLoader: JdiClassLoader
  ): Safe[(List[StringReference], List[Value])] = {
    val thisObjectOpt = Option(frame.thisObject) // this object can be null
    def extractVariablesFromFrame()
        : Safe[(List[StringReference], List[Value])] = {
      val variables: List[LocalVariable] =
        frame.visibleVariables().asScala.toList
      val variableNames =
        variables.map(_.name).map(classLoader.mirrorOf).traverse
      val variableValues =
        variables
          .map(frame.getValue)
          .map(value => boxIfPrimitive(value, classLoader, classLoader.thread))
          .traverse
      Safe.join(variableNames, variableValues)
    }

    // Only useful in Scala 2
    def extractFields(
        thisObject: ObjectReference
    ): Safe[(List[StringReference], List[Value])] = {
      val fields = thisObject.referenceType.fields.asScala.toList
      val fieldNames = fields.map(_.name).map(classLoader.mirrorOf).traverse
      val fieldValues = fields
        .map(field => thisObject.getValue(field))
        .map(value => boxIfPrimitive(value, classLoader, classLoader.thread))
        .traverse
      Safe.join(fieldNames, fieldValues)
    }

    val isScala2 = scalaVersion.startsWith("2")
    for {
      (variableNames, variableValues) <- extractVariablesFromFrame()
      // Currently we only need to load the fields for the Scala 2
      // expression evaluator.
      // It is dangerous because local values can shadow fields
      // TODO: adapt Scala 2 expression compiler
      (fieldNames, fieldValues) <- thisObjectOpt
        .filter(_ => isScala2)
        .map(extractFields)
        .getOrElse(Safe.lift((Nil, Nil)))
      // If `this` is a value class then the breakpoint must be in
      // a generated method of its companion object which takes
      // the erased value`$this` as argument.
      thisObjectValue = thisObjectOpt
        .filter(_ => !variableNames.exists(_.value == "$this"))
      thisObjectName <- thisObjectValue
        .map(_ => classLoader.mirrorOf("$this"))
        .traverse
    } yield {
      val names = variableNames ++ fieldNames ++ thisObjectName
      val values = variableValues ++ fieldValues ++ thisObjectValue
      (names, values)
    }
  }

  private def updateVariables(
      variableArray: JdiArray,
      thread: ThreadReference,
      depth: Int
  ): Safe[Unit] = {
    def localVariables(): List[LocalVariable] =
      // we must get a new StackFrame object after each invocation
      thread.frame(depth).visibleVariables().asScala.toList

    val unboxedValues = localVariables()
      .zip(variableArray.getValues)
      .map { case (variable, value) =>
        if (isPrimitive(variable)) unboxIfPrimitive(value, thread)
        else Safe.lift(value)
      }
      .traverse

    for (values <- unboxedValues)
      yield {
        for ((variable, value) <- localVariables().zip(values)) {
          thread.frame(depth).setValue(variable, value)
        }
      }
  }

  private def findClassLoader(
      thread: ThreadReference
  ): JdiClassLoader = {
    val someClass = thread.virtualMachine.allClasses.asScala
      .find(_.classLoader() != null)
      .head
    JdiClassLoader(someClass.classLoader, thread)
  }

  private def boxIfPrimitive(
      value: Value,
      classLoader: JdiClassLoader,
      thread: ThreadReference
  ): Safe[Value] =
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
      case value => Safe.lift(value)
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

  private def unboxIfPrimitive(
      value: Value,
      thread: ThreadReference
  ): Safe[Value] = {
    value match {
      case ref: ObjectReference =>
        val typeName = ref.referenceType().name()
        unboxMethods
          .get(typeName)
          .map(methodName => new JdiObject(ref, thread).invoke(methodName, Nil))
          .getOrElse(Safe.lift(value))
      case _ => Safe.lift(value)
    }
  }

  private def isPrimitive(variable: LocalVariable): Boolean =
    variable.`type`().isInstanceOf[PrimitiveType]

}
