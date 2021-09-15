package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi._

import scala.util.Try

class JdiClassObject(
    reference: ClassObjectReference,
    classLoader: JdiClassLoader,
    thread: ThreadReference
) extends JdiObject(reference, thread) {

  def newInstance(args: List[ObjectReference]): Safe[JdiObject] = {
    val parameterTypes = args.map(_.referenceType.classObject())
    for {
      objects <- JdiArray("java.lang.Object", args.size, classLoader)
      _ = objects.setValues(args)
      constructor <- invoke("getConstructor", parameterTypes)
        .map(_.asInstanceOf[ObjectReference])
        .map(new JdiObject(_, thread))
      jdiObject <- constructor
        .invoke("newInstance", List(objects.reference))
        .map(_.asInstanceOf[ObjectReference])
        .map(new JdiObject(_, thread))
    } yield jdiObject
  }

  def invokeStatic(
      methodName: String,
      args: List[ObjectReference]
  ): Safe[Value] = {
    val parameterTypes = args.map(_.referenceType.classObject())
    for {
      methodNameValue <- classLoader.mirrorOf(methodName)
      method <- invoke(
        "getMethod",
        methodNameValue :: parameterTypes
      )
        .map(_.asInstanceOf[ObjectReference])
        .map(new JdiObject(_, thread))
      result <- method.invoke("invoke", List(null) ++ args)
    } yield result
  }
}
