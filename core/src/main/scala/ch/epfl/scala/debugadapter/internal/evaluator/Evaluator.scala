package ch.epfl.scala.debugadapter.internal.evaluator

import com.microsoft.java.debug.core.adapter.ISourceLookUpProvider
import com.sun.jdi._

import java.nio.file.Files
import java.util.concurrent.CompletableFuture
import scala.collection.JavaConverters._
import scala.util.Try

private[evaluator] class Evaluator(sourceLookUpProvider: ISourceLookUpProvider, expressionCompiler: ExpressionCompiler) {
  def evaluate(
    expression: String,
    thread: ThreadReference,
    frame: StackFrame
  ): CompletableFuture[Value] = {
    val vm = thread.virtualMachine()
    val thisObject = Option(frame.thisObject())

    val location = frame.location()
    val sourcePath = location.sourcePath()
    val line = location.lineNumber()
    val fqcn = location.declaringType().name()

    val uri = sourceLookUpProvider.getSourceFileURI(fqcn, sourcePath)
    val content = sourceLookUpProvider.getSourceContents(uri)

    val expressionDir = Files.createTempDirectory("expr-eval")
    val expressionId = java.util.UUID.randomUUID.toString.replace("-", "")
    val expressionClassName: String = s"Expression$expressionId"
    val valuesByNameIdentName: String = s"valuesByName$expressionId"

    val expressionFqcn = (fqcn.split("\\.").dropRight(1) :+ expressionClassName).mkString(".")

    var error: Option[String] = None
    val result = for {
      classLoader <- findClassLoader(vm).flatMap(JdiClassLoader(_, thread))
      variables = frame.visibleVariables().asScala
      variableNames = variables.map(_.name()).map(vm.mirrorOf).toList
      variableValues = variables.map(frame.getValue).flatMap(value => boxIfNeeded(value, classLoader, thread)).toList
      fields = thisObject.map(_.referenceType().fields().asScala).getOrElse(List())
      fieldNames = fields.map(_.name()).map(vm.mirrorOf).toList
      fieldValues = thisObject.map(thiz => fields.map(field => thiz.getValue(field)).flatMap(value => boxIfNeeded(value, classLoader, thread))).getOrElse(List())
      thisObjectName = thisObject.map(_ => vm.mirrorOf("$this"))
      names = variableNames ++ fieldNames ++ thisObjectName.map(Seq(_)).getOrElse(Seq())
      values = variableValues ++ fieldValues ++ thisObject.map(Seq(_)).getOrElse(Seq())
      systemClass <- classLoader.loadClass("java.lang.System")
      classPath <- systemClass
        .invokeStatic("getProperty", List(vm.mirrorOf("java.class.path")))
        .map(_.toString)
        .map(_.drop(1).dropRight(1)) // remove quotation marks
      expressionClassPath = expressionDir.toUri.toString
      compiledSuccessfully = expressionCompiler.compile(
        expressionDir,
        expressionClassName,
        valuesByNameIdentName,
        classPath,
        content,
        line,
        expression,
        names.map(_.value()).toSet,
        errorMessage => error = Some(errorMessage)
      )
      _ <- if (compiledSuccessfully) Some(()) else None
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
      expression <- expressionClass.newInstance(List())
      namesArray <- JdiArray("java.lang.String", names.size, classLoader, thread)
      valuesArray <- JdiArray("java.lang.Object", values.size, classLoader, thread) // add boxing
      _ = namesArray.setValues(names)
      _ = valuesArray.setValues(values)
      result <- expression.invoke("evaluate", List(namesArray.reference, valuesArray.reference))
    } yield result

    result match {
      case Some(value) =>
        CompletableFuture.completedFuture(value)
      case None => error match {
        case Some(message) =>
          throw new Exception(message)
        case None =>
          throw new Exception("Unable to evaluate the expression")
      }
    }
  }

  private def findClassLoader(vm: VirtualMachine) =
    vm.allClasses().asScala.find(_.classLoader() != null).map(_.classLoader())

  private def boxIfNeeded(value: Value, classLoader: JdiClassLoader, thread: ThreadReference) = value match {
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

  def invokeMethod(
    thisContext: ObjectReference,
    methodName: String,
    signature: String,
    args: Array[Value],
    thread: ThreadReference,
    invokeSuper: Boolean
  ): CompletableFuture[Value] = {
    val result = for {
      obj <- Try(new JdiObject(thisContext, thread)).toOption
      result <- obj.invoke(methodName, signature, if (args == null) List() else args.toList)
    } yield result

    result match {
      case Some(value) =>
        CompletableFuture.completedFuture(value)
      case None =>
        throw new Exception("Unable to evaluate the expression")
    }
  }
}
