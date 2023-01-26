package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi._
import scala.collection.JavaConverters.*

private[internal] object JdiClassLoader {
  def fromFrame(frame: FrameReference): Safe[JdiClassLoader] = Safe {
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

private[internal] case class JdiClassLoader(
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
