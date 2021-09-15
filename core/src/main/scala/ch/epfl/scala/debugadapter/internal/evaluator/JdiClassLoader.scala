package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi._

import scala.util.Try

private[evaluator] object JdiClassLoader {
  def apply(
      classLoader: ClassLoaderReference,
      thread: ThreadReference
  ): Option[JdiClassLoader] =
    for {
      classLoaderType <- Try(
        classLoader.referenceType().asInstanceOf[ClassType]
      ).toOption
      loadClassMethod <- loadClassMethod(classLoaderType)
    } yield new JdiClassLoader(classLoader, loadClassMethod, thread)

  private def loadClassMethod(classLoaderType: ReferenceType) =
    method(
      "loadClass",
      "(Ljava/lang/String;)Ljava/lang/Class;",
      classLoaderType
    )
}

private[evaluator] case class JdiClassLoader(
    classLoaderRef: ClassLoaderReference,
    loadClassMethod: Method,
    thread: ThreadReference
) {
  val vm = thread.virtualMachine()

  def loadClass(name: String): Option[JdiClassObject] =
    invokeMethod(
      classLoaderRef,
      loadClassMethod,
      List(vm.mirrorOf(name)),
      thread
    )
      .map(_.asInstanceOf[ClassObjectReference])
      .map(new JdiClassObject(_, this, thread))
}
