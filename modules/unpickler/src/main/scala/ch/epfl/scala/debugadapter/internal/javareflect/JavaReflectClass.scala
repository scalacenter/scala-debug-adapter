package ch.epfl.scala.debugadapter.internal.javareflect

import ch.epfl.scala.debugadapter.internal.binary
import scala.util.matching.Regex
import scala.jdk.CollectionConverters.*

class JavaReflectClass(cls: Class[?], sourceLineMap: Map[MethodSig, Seq[Int]]) extends binary.ClassType:

  override def name: String = cls.getTypeName

  override def superclass = Option(cls.getSuperclass).map(JavaReflectClass(_))

  override def interfaces = cls.getInterfaces.toList.map(JavaReflectClass(_))

  override def toString: String = cls.toString

  override def isInterface: Boolean = cls.isInterface()

  def declaredMethods: Seq[binary.Method] =
    val methods = cls.getDeclaredMethods.map { m =>
      val sig = JavaReflectUtils.signature(m)
      val sourceLines = sourceLineMap(sig)
      JavaReflectMethod(m, sourceLines)
    }
    val constructors = cls.getDeclaredConstructors.map(JavaReflectConstructor(_, Seq.empty))
    methods ++ constructors

object JavaReflectClass:
  def apply(cls: Class[?], sourceLineMap: Map[MethodSig, Seq[Int]]): JavaReflectClass =
    new JavaReflectClass(cls, sourceLineMap)

  def apply(cls: Class[?]): JavaReflectClass = new JavaReflectClass(cls, Map.empty)
