package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi.{ClassLoaderReference, ClassObjectReference, ObjectReference, ThreadReference, Value}

import java.util.concurrent.CompletableFuture
import scala.tools.nsc.interactive.ExpressionCompiler

object Evaluator {
  def evaluate(expression: String, objectReference: ObjectReference, thread: ThreadReference): CompletableFuture[Value] = {
    val vm = thread.virtualMachine()
    val result = for {
      classLoader <- classLoader(objectReference).flatMap(JdiClassLoader(_, thread))
      systemClass <- classLoader.loadClass("java.lang.System")
      classPath <- systemClass
        .invokeStatic("getProperty", List(vm.mirrorOf("java.class.path")))
        .map(_.toString)
      expressionCompiler <- Some(ExpressionCompiler(classPath))
      expressionClassPath = s"file://${expressionCompiler.dir.toString}/"
      _ <- Some(expressionCompiler.compile(expression))
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
      expressionClass <- urlClassLoader.loadClass("Expression")
      expression <- expressionClass.newInstance(List())
      result <- expression.invoke("evaluate", List())
    } yield result

    // Handle error
    CompletableFuture.completedFuture(result.get)
  }
}
