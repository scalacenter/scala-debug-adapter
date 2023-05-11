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

  @inline private def loadThenGetType(className: String): Safe[ClassType] =
    loadClass(className).map(_.cls)

  @inline def mirrorOf(str: String): Safe[JdiString] =
    Safe(thread.virtualMachine.mirrorOf(str)).map(new JdiString(_, thread))

  @inline def mirrorOf(boolean: Boolean): JdiValue =
    JdiValue(thread.virtualMachine.mirrorOf(boolean), thread)

  @inline def mirrorOf(byte: Byte): JdiValue =
    JdiValue(thread.virtualMachine.mirrorOf(byte), thread)

  @inline def mirrorOf(char: Char): JdiValue =
    JdiValue(thread.virtualMachine.mirrorOf(char), thread)

  @inline def mirrorOf(double: Double): JdiValue =
    JdiValue(thread.virtualMachine.mirrorOf(double), thread)

  @inline def mirrorOf(float: Float): JdiValue =
    JdiValue(thread.virtualMachine.mirrorOf(float), thread)

  @inline def mirrorOf(int: Int): JdiValue =
    JdiValue(thread.virtualMachine.mirrorOf(int), thread)

  @inline def mirrorOf(long: Long): JdiValue =
    JdiValue(thread.virtualMachine.mirrorOf(long), thread)

  @inline def mirrorOf(short: Short): JdiValue =
    JdiValue(thread.virtualMachine.mirrorOf(short), thread)

  def mirrorOfAnyVal(value: AnyVal): Safe[JdiValue] = Safe {
    value match {
      case d: Double => mirrorOf(d)
      case f: Float => mirrorOf(f)
      case l: Long => mirrorOf(l)
      case i: Int => mirrorOf(i)
      case s: Short => mirrorOf(s)
      case c: Char => mirrorOf(c)
      case b: Byte => mirrorOf(b)
      case b: Boolean => mirrorOf(b)
    }
  }

  def boxIfPrimitive(value: JdiValue): Safe[JdiValue] =
    value.value match {
      case value: BooleanValue => box(value.value)
      case value: CharValue => box(value.value)
      case value: DoubleValue => box(value.value)
      case value: FloatValue => box(value.value)
      case value: IntegerValue => box(value.value)
      case value: LongValue => box(value.value)
      case value: ShortValue => box(value.value)
      case value: ByteValue => box(value.value)
      case _ => Safe(value)
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
      array <- arrayClass
        .invokeStatic("newInstance", "(Ljava/lang/Class;I)Ljava/lang/Object;", Seq(arrayTypeClass, mirrorOf(values.size)))
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
  def apply(ref: Value, thread: ThreadReference): JdiClassLoader =
    new JdiClassLoader(ref.asInstanceOf[ClassLoaderReference], thread)
}
