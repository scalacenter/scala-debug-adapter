package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi._

private[evaluator] object JdiClassLoader {
  def apply(
      classLoader: ClassLoaderReference,
      thread: ThreadReference
  ): JdiClassLoader = {
    val classLoaderType = classLoader.referenceType
    val loadClassMethod = method(
      "loadClass",
      "(Ljava/lang/String;)Ljava/lang/Class;",
      classLoaderType
    )
    new JdiClassLoader(classLoader, loadClassMethod, thread)
  }
}

private[evaluator] case class JdiClassLoader(
    classLoaderRef: ClassLoaderReference,
    loadClassMethod: Method,
    thread: ThreadReference
) {
  def loadClass(name: String): Safe[JdiClassObject] = {
    for {
      nameValue <- mirrorOf(name)
      classObject <- invokeMethod(
        classLoaderRef,
        loadClassMethod,
        List(nameValue),
        thread
      )
    } yield {
      new JdiClassObject(
        classObject.asInstanceOf[ClassObjectReference],
        this,
        thread
      )
    }
  }

  def mirrorOf(str: String): Safe[StringReference] = {
    Safe(thread.virtualMachine.mirrorOf(str))
  }
}
