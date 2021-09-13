package ch.epfl.scala.debugadapter.internal.evaluator

import com.microsoft.java.debug.core.adapter.{
  IDebugAdapterContext,
  ISourceLookUpProvider
}
import com.sun.jdi._

import java.nio.file.{Files, Path}
import java.util.concurrent.CompletableFuture
import scala.collection.JavaConverters._
import scala.concurrent.duration._

private case class EvaluatorContext(
    thread: ThreadReference,
    frame: StackFrame,
    vm: VirtualMachine,
    classLoader: JdiClassLoader
)

private[internal] class ExpressionEvaluator(
    sourceLookUpProvider: ISourceLookUpProvider,
    expressionCompiler: ExpressionCompiler
) {
  def evaluate(
      expression: String,
      thread: ThreadReference,
      frame: StackFrame
  )(debugContext: IDebugAdapterContext): CompletableFuture[Value] = {
    val vm = thread.virtualMachine()

    val location = frame.location()
    val sourcePath = location.sourcePath()
    val breakpointLine = location.lineNumber()
    val fqcn = location.declaringType().name()

    val uri = sourceLookUpProvider.getSourceFileURI(fqcn, sourcePath)
    val content = sourceLookUpProvider.getSourceContents(uri)

    val expressionDir = Files.createTempDirectory("expr-eval")
    val expressionId = java.util.UUID.randomUUID.toString.replace("-", "")
    val expressionClassName: String = s"Expression$expressionId"
    val valuesByNameIdentName: String = s"valuesByName$expressionId"

    val expressionFqcn =
      (fqcn.split("\\.").dropRight(1) :+ expressionClassName).mkString(".")

    try {
      var error: Option[String] = None
      val result = for {
        classLoader <- findClassLoader(vm).flatMap(JdiClassLoader(_, thread))
        context = EvaluatorContext(thread, frame, vm, classLoader)
        (names, values) = extractValuesAndNames(context)
        systemClass <- classLoader.loadClass("java.lang.System")
        classPath <- systemClass
          .invokeStatic("getProperty", List(vm.mirrorOf("java.class.path")))
          .map(_.toString)
          .map(_.drop(1).dropRight(1)) // remove quotation marks
        expressionClassPath = expressionDir.toUri.toString
        compiledSuccessfully = expressionCompiler
          .compile(
            expressionDir,
            expressionClassName,
            valuesByNameIdentName,
            classPath,
            content,
            breakpointLine,
            expression,
            names.map(_.value()).toSet,
            errorMessage => error = Some(errorMessage),
            2 seconds
          )
        _ <- if (compiledSuccessfully) Some(()) else None
        // if everything went smooth we can load our expression class
        expressionClass <- loadExpressionClass(
          context,
          expressionDir,
          expressionFqcn
        )
        expression <- expressionClass.newInstance(List())
        namesArray <- JdiArray(
          "java.lang.String",
          names.size,
          classLoader,
          thread
        )
        valuesArray <- JdiArray(
          "java.lang.Object",
          values.size,
          classLoader,
          thread
        ) // add boxing
        _ = namesArray.setValues(names)
        _ = valuesArray.setValues(values)
        result <- expression.invoke(
          "evaluate",
          List(namesArray.reference, valuesArray.reference)
        )
      } yield result

      result match {
        case Some(value) =>
          CompletableFuture.completedFuture(value)
        case None =>
          error match {
            case Some(message) =>
              throw new Exception(message)
            case None =>
              throw new Exception("Unable to evaluate the expression")
          }
      }
    } finally {
      debugContext.getStackFrameManager.reloadStackFrames(thread)
    }
  }

  /**
   * In order to load the previously compiled Expression class, we need to
   * first load and instantiate URL with expressionClassPath
   * and then URLClassLoader with the url created before.
   */
  private def loadExpressionClass(
      context: EvaluatorContext,
      expressionDir: Path,
      expressionFqcn: String
  ): Option[JdiClassObject] = {
    import context._
    val expressionClassPath = expressionDir.toUri.toString
    for {
      url <- classLoader
        .loadClass("java.net.URL")
        .flatMap(_.newInstance(List(vm.mirrorOf(expressionClassPath))))
      urls <- JdiArray("java.net.URL", 1, classLoader, thread)
      _ <- urls.setValue(0, url.reference)
      urlClassLoader <- classLoader
        .loadClass("java.net.URLClassLoader")
        .flatMap(_.newInstance(List(urls.reference)))
        .map(_.reference.asInstanceOf[ClassLoaderReference])
        .flatMap(JdiClassLoader(_, thread))
      expressionClass <- urlClassLoader.loadClass(expressionFqcn)
    } yield expressionClass
  }

  /**
   * Extract all values and their corresponding names which are visible in current scope.
   * Values consist of:
   * - variables from stack frame
   * - fields from this object
   * @return Tuple of extracted names and values
   */
  private def extractValuesAndNames(
      context: EvaluatorContext
  ): (List[StringReference], List[Value]) = {
    import context._
    val thisObjectOpt = Option(frame.thisObject())

    def extractVariablesFromFrame() = {
      val variables: List[LocalVariable] =
        frame.visibleVariables().asScala.toList
      val variableNames = variables.map(_.name()).map(vm.mirrorOf)
      val variableValues = variables
        .map(frame.getValue)
        .flatMap(value => boxIfNeeded(value, classLoader, thread))
      (variableNames, variableValues)
    }

    def extractFieldsFromThisObject() =
      thisObjectOpt
        .map { thisObject =>
          val fields = thisObject.referenceType().fields().asScala.toList
          val fieldNames = fields.map(_.name()).map(vm.mirrorOf)
          val fieldValues = fields
            .map(field => thisObject.getValue(field))
            .flatMap(value => boxIfNeeded(value, classLoader, thread))
          (fieldNames, fieldValues)
        }
        .getOrElse((Nil, Nil))

    val (variableNames, variableValues) = extractVariablesFromFrame()
    val (fieldNames, fieldValues) = extractFieldsFromThisObject()
    val thisObjectName = thisObjectOpt.map(_ => vm.mirrorOf("$this"))
    val names = variableNames ++ fieldNames ++ thisObjectName.toList
    val values = variableValues ++ fieldValues ++ thisObjectOpt.toList
    (names, values)
  }

  private def findClassLoader(vm: VirtualMachine) =
    vm.allClasses().asScala.find(_.classLoader() != null).map(_.classLoader())

  private def boxIfNeeded(
      value: Value,
      classLoader: JdiClassLoader,
      thread: ThreadReference
  ) = value match {
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
    case value => Some(value)
  }
}
