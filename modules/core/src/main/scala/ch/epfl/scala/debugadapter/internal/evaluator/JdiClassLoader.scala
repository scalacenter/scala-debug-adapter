package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi._

import java.nio.file.Path
import RuntimeEvaluatorExtractors.IsAnyVal

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
    JdiValue(thread.virtualMachine.mirrorOf(boolean), thread)

  def mirrorOf(byte: Byte): JdiValue =
    JdiValue(thread.virtualMachine.mirrorOf(byte), thread)

  def mirrorOf(char: Char): JdiValue =
    JdiValue(thread.virtualMachine.mirrorOf(char), thread)

  def mirrorOf(double: Double): JdiValue =
    JdiValue(thread.virtualMachine.mirrorOf(double), thread)

  def mirrorOf(float: Float): JdiValue =
    JdiValue(thread.virtualMachine.mirrorOf(float), thread)

  def mirrorOf(int: Int): JdiValue =
    JdiValue(thread.virtualMachine.mirrorOf(int), thread)

  def mirrorOf(long: Long): JdiValue =
    JdiValue(thread.virtualMachine.mirrorOf(long), thread)

  def mirrorOf(short: Short): JdiValue =
    JdiValue(thread.virtualMachine.mirrorOf(short), thread)

  def mirrorOfVoid(): JdiValue =
    JdiValue(thread.virtualMachine.mirrorOfVoid(), thread)

  def mirrorOfAnyVal(value: AnyVal): JdiValue = value match {
    case d: Double => mirrorOf(d)
    case f: Float => mirrorOf(f)
    case l: Long => mirrorOf(l)
    case i: Int => mirrorOf(i)
    case s: Short => mirrorOf(s)
    case c: Char => mirrorOf(c)
    case b: Byte => mirrorOf(b)
    case b: Boolean => mirrorOf(b)
  }

  def mirrorOfLiteral(value: Any): Safe[JdiValue] = value match {
    case IsAnyVal(value) => Safe(mirrorOfAnyVal(value))
    case value: String => mirrorOf(value)
    case () => Safe(mirrorOfVoid())
    case _ => Safe.failed(new IllegalArgumentException(s"Unsupported literal $value"))
  }

  def boxIfPrimitive(value: JdiValue): Safe[JdiValue] =
    value.value match {
      case _: PrimitiveValue => box(value)
      case _ => Safe(value)
    }

  def box(value: JdiValue): Safe[JdiObject] = {
    for {
      jdiValue <- value.value match {
        case c: CharValue => Safe(value)
        case _ => mirrorOf(value.value.toString)
      }
      _ = getClass
      (className, sig) = value.value match {
        case _: BooleanValue => ("java.lang.Boolean", "(Ljava/lang/String;)Ljava/lang/Boolean;")
        case _: ByteValue => ("java.lang.Byte", "(Ljava/lang/String;)Ljava/lang/Byte;")
        case _: CharValue => ("java.lang.Character", "(C)Ljava/lang/Character;")
        case _: DoubleValue => ("java.lang.Double", "(Ljava/lang/String;)Ljava/lang/Double;")
        case _: FloatValue => ("java.lang.Float", "(Ljava/lang/String;)Ljava/lang/Float;")
        case _: IntegerValue => ("java.lang.Integer", "(Ljava/lang/String;)Ljava/lang/Integer;")
        case _: LongValue => ("java.lang.Long", "(Ljava/lang/String;)Ljava/lang/Long;")
        case _: ShortValue => ("java.lang.Short", "(Ljava/lang/String;)Ljava/lang/Short;")
      }
      clazz <- loadClass(className)
      objectRef <- clazz.invokeStatic("valueOf", sig, List(jdiValue))
    } yield objectRef.asObject
  }

  def boxUnboxOnNeed(
      expected: Seq[Type],
      received: Seq[JdiValue]
  ): Safe[Seq[JdiValue]] = {
    expected
      .zip(received)
      .map { case (expect: Type, got: JdiValue) =>
        (expect, got.value) match {
          case (argType: ReferenceType, arg: PrimitiveValue) => boxIfPrimitive(got)
          case (argType: PrimitiveType, arg: ObjectReference) => got.unboxIfPrimitive
          case (argType, arg) => Safe(got)
        }
      }
      .traverse
  }

  def boxUnboxOnNeed(
      expected: java.util.List[Type],
      received: Seq[JdiValue]
  ): Safe[Seq[JdiValue]] = boxUnboxOnNeed(expected.asScalaSeq, received)

  def createArray(arrayType: String, values: Seq[JdiValue]): Safe[JdiArray] =
    for {
      arrayTypeClass <- loadClass(arrayType)
      arrayClass <- loadClass("java.lang.reflect.Array")
      array <- arrayClass
        .invokeStatic(
          "newInstance",
          "(Ljava/lang/Class;I)Ljava/lang/Object;",
          Seq(arrayTypeClass, mirrorOf(values.size))
        )
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
