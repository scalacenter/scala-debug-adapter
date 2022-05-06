package ch.epfl.scala.debugadapter.internal.evaluator

import com.microsoft.java.debug.core.adapter.ISourceLookUpProvider
import com.sun.jdi._

import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.util.Try

private[internal] class ExpressionEvaluator(
    classPath: Seq[Path],
    sourceLookUpProvider: ISourceLookUpProvider,
    driver: EvaluationDriver
) {
  private val classPathString =
    classPath.mkString(File.pathSeparator)

  def evaluate(
      expression: String,
      thread: ThreadReference,
      frame: StackFrame
  ): Try[Value] = {
    val location = frame.location()
    val sourcePath = location.sourcePath()
    val breakpointLine = location.lineNumber()
    val fqcn = location.declaringType().name()

    val uri = sourceLookUpProvider.getSourceFileURI(fqcn, sourcePath)
    val content = sourceLookUpProvider.getSourceContents(uri)

    val randomId = java.util.UUID.randomUUID.toString.replace("-", "")
    val expressionDir =
      Files.createTempDirectory(s"scala-debug-adapter-$randomId")
    val expressionClassName = s"Expression$randomId"

    val expressionFqcn =
      (fqcn.split("\\.").dropRight(1) :+ expressionClassName).mkString(".")
    var error: Option[String] = None
    val classLoader = findClassLoader(thread)
    val evaluatedValue = for {
      // must be called before any invocation otherwise
      // it throws an InvalidStackFrameException
      (names, values) <- extractValuesAndNames(frame, classLoader)
      compiled = driver
        .run(
          expressionDir,
          expressionClassName,
          classPathString,
          content,
          breakpointLine,
          expression,
          names.map(_.value()).toSet,
          errorMessage => error = Some(errorMessage),
          5.seconds
        )
      _ = {
        if (!compiled)
          throw new ExpressionCompilationFailed(error.getOrElse(""))
      }
      // // if everything went smooth we can load our expression class
      namesArray <-
        JdiArray("java.lang.String", names.size, classLoader)
      valuesArray <-
        JdiArray("java.lang.Object", values.size, classLoader) // add boxing
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
    } yield evaluatedValue
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
          .map(value => boxIfNeeded(value, classLoader, classLoader.thread))
          .traverse
      Safe.join(variableNames, variableValues)
    }

    def extractFieldsFromThisObject(
        thisObject: ObjectReference
    ): Safe[(List[StringReference], List[Value])] = {
      val fields = thisObject.referenceType.fields.asScala.toList
      val fieldNames = fields.map(_.name).map(classLoader.mirrorOf).traverse
      val fieldValues = fields
        .map(field => thisObject.getValue(field))
        .map(value => boxIfNeeded(value, classLoader, classLoader.thread))
        .traverse
      Safe.join(fieldNames, fieldValues)
    }

    for {
      (variableNames, variableValues) <- extractVariablesFromFrame()
      (fieldNames, fieldValues) <-
        thisObjectOpt
          .map(extractFieldsFromThisObject)
          .getOrElse(Safe.lift((Nil, Nil)))
      thisObjectName <- thisObjectOpt match {
        case Some(thisObject) =>
          classLoader.mirrorOf("$this").map(Some.apply)
        case None => Safe.lift(None)
      }
    } yield {
      val names = variableNames ++ fieldNames ++ thisObjectName
      val values = variableValues ++ fieldValues ++ thisObjectOpt
      (names, values)
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

  private def boxIfNeeded(
      value: Value,
      classLoader: JdiClassLoader,
      thread: ThreadReference
  ): Safe[Value] = value match {
    case value: BooleanValue =>
      JdiPrimitive.boxed(value.value(), classLoader, thread).map(_.reference)
    case value: CharValue =>
      JdiPrimitive.boxed(value.value(), classLoader, thread).map(_.reference)
    case value: DoubleValue =>
      JdiPrimitive.boxed(value.value(), classLoader, thread).map(_.reference)
    case value: FloatValue =>
      JdiPrimitive.boxed(value.value(), classLoader, thread).map(_.reference)
    case value: IntegerValue =>
      JdiPrimitive.boxed(value.value(), classLoader, thread).map(_.reference)
    case value: LongValue =>
      JdiPrimitive.boxed(value.value(), classLoader, thread).map(_.reference)
    case value: ShortValue =>
      JdiPrimitive.boxed(value.value(), classLoader, thread).map(_.reference)
    case value => Safe(value)
  }
}
