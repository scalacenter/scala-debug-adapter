package ch.epfl.scala.debugadapter.internal.javareflect

import java.lang.reflect.Method
import java.lang.reflect.Constructor
import ch.epfl.scala.debugadapter.internal.binary.SignedName

object JavaReflectUtils:
  val primitiveSigs = Map[Class[?], String](
    classOf[Byte] -> "B",
    classOf[Char] -> "C",
    classOf[Short] -> "S",
    classOf[Int] -> "I",
    classOf[Long] -> "J",
    classOf[Float] -> "F",
    classOf[Double] -> "D",
    classOf[Boolean] -> "Z",
    classOf[Unit] -> "V"
  )

  def signature(method: Method): SignedName =
    val params = method.getParameterTypes.map(signature)
    val returnType = signature(method.getReturnType)
    SignedName(method.getName, s"(${params.mkString})$returnType")

  def signature(ctr: Constructor[?]): SignedName =
    val params = ctr.getParameterTypes.map(signature)
    SignedName("<init>", s"(${params.mkString})V")

  def signature(cls: Class[?]): String =
    if cls.isPrimitive then primitiveSigs(cls)
    else if cls.isArray then s"[" + signature(cls.getComponentType)
    else "L" + cls.getName.replace('.', '/') + ";"
