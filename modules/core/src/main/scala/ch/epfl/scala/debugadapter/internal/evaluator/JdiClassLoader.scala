package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi._

import java.nio.file.Path

private[internal] class JdiClassLoader(
    reference: ClassLoaderReference,
    thread: ThreadReference
) extends JdiObject(reference, thread) {

  private def loadClassClass: Safe[JdiClass] =
    for {
      classClassName <- mirrorOf("java.lang.Class")
      classClass <- invoke("loadClass", "(Ljava/lang/String;)Ljava/lang/Class;", Seq(classClassName))
    } yield classClass.asClass

  def loadClass(className: String): Safe[JdiClass] =
    for {
      classNameValue <- mirrorOf(className)
      classClass <- loadClassClass
      classObject <- classClass.invokeStatic(
        "forName",
        "(Ljava/lang/String;ZLjava/lang/ClassLoader;)Ljava/lang/Class;",
        Seq(classNameValue, mirrorOf(true), this)
      )
    } yield classObject.asClass

  def mirrorOf(str: String): Safe[JdiString] =
    Safe(thread.virtualMachine.mirrorOf(str)).map(new JdiString(_, thread))

  def mirrorOf(boolean: Boolean): JdiValue =
    new JdiValue(thread.virtualMachine.mirrorOf(boolean), thread)

  def mirrorOf(integer: Int): JdiValue =
    new JdiValue(thread.virtualMachine.mirrorOf(integer), thread)

  def boxIfPrimitive(value: JdiValue): Safe[JdiValue] =
    value.value match {
      case value: BooleanValue => box(value.value)
      case value: CharValue => box(value.value)
      case value: DoubleValue => box(value.value)
      case value: FloatValue => box(value.value)
      case value: IntegerValue => box(value.value)
      case value: LongValue => box(value.value)
      case value: ShortValue => box(value.value)
      case value => Safe(JdiValue(value, thread))
    }

  def box(value: AnyVal): Safe[JdiObject] =
    for {
      jdiValue <- mirrorOf(value.toString)
      _ = getClass
      (className, sig) = value match {
        case _: Boolean => ("java.lang.Boolean", "(Ljava/lang/String;)Ljava/lang/Boolean;")
        case _: Byte => ("java.lang.Byte", "(Ljava/lang/String;)Ljava/lang/Byte;")
        case _: Char => ("java.lang.Character", "(Ljava/lang/String;)Ljava/lang/Character;")
        case _: Double => ("java.lang.Double", "(Ljava/lang/String;)Ljava/lang/Double;")
        case _: Float => ("java.lang.Float", "(Ljava/lang/String;)Ljava/lang/Float;")
        case _: Int => ("java.lang.Integer", "(Ljava/lang/String;)Ljava/lang/Integer;")
        case _: Long => ("java.lang.Long", "(Ljava/lang/String;)Ljava/lang/Long;")
        case _: Short => ("java.lang.Short", "(Ljava/lang/String;)Ljava/lang/Short;")
      }
      clazz <- loadClass(className)
      objectRef <- clazz.invokeStatic("valueOf", sig, List(jdiValue))
    } yield objectRef.asObject

  def createArray(arrayType: String, values: Seq[JdiValue]): Safe[JdiArray] =
    for {
      arrayTypeClass <- loadClass(arrayType)
      arrayClass <- loadClass("java.lang.reflect.Array")
      size = mirrorOf(values.size)
      array <- arrayClass
        .invokeStatic("newInstance", "(Ljava/lang/Class;I)Ljava/lang/Object;", Seq(arrayTypeClass, size))
        .map(_.asArray)
    } yield {
      array.setValues(values)
      array
    }

  def createChildLoader(classPathEntry: Path): Safe[JdiClassLoader] =
    for {
      classPathValue <- mirrorOf(classPathEntry.toUri.toString)
      urlClass <- loadClass("java.net.URL")
      url <- urlClass.newInstance("(Ljava/lang/String;)V", List(classPathValue))
      urls <- createArray("java.net.URL", Seq(url))
      classOfUrlClassLoader <- loadClass("java.net.URLClassLoader")
      urlClassLoader <- classOfUrlClassLoader.newInstance("([Ljava/net/URL;Ljava/lang/ClassLoader;)V", List(urls, this))
    } yield urlClassLoader.asClassLoader
}

private[internal] object JdiClassLoader {
  def apply(ref: ClassLoaderReference, thread: ThreadReference): JdiClassLoader =
    new JdiClassLoader(ref, thread)
}
