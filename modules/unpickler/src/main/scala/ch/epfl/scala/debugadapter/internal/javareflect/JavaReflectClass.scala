package ch.epfl.scala.debugadapter.internal.javareflect

import ch.epfl.scala.debugadapter.internal.binary
import scala.util.matching.Regex
import scala.jdk.CollectionConverters.*

class JavaReflectClass(cls: Class[?], sourceLineMap: Map[MethodSig, Seq[Int]]) extends binary.ClassType:
  override def name: String = cls.getTypeName
  override def superclass = Option(cls.getSuperclass).map(JavaReflectClass(_))
  override def interfaces = cls.getInterfaces.toList.map(JavaReflectClass(_))
  override def isInterface: Boolean = cls.isInterface
  override def sourceLines: Seq[Int] =
    val distinctLines = sourceLineMap.values.flatten.toSeq.distinct
    if distinctLines.size > 1 then Seq(distinctLines.min, distinctLines.max)
    else distinctLines

  override def toString: String = cls.toString

  def declaredMethodsAndConstructors: Seq[binary.Method] =
    declaredConstructors ++ declaredMethods

  private def declaredConstructors: Seq[binary.Method] =
    cls.getDeclaredConstructors.map { c =>
      val sig = JavaReflectUtils.signature(c)
      val sourceLines = sourceLineMap(sig)
      JavaReflectConstructor(c, sourceLines)
    }

  private def declaredMethods: Seq[binary.Method] =
    cls.getDeclaredMethods.map { m =>
      val sig = JavaReflectUtils.signature(m)
      val sourceLines = sourceLineMap(sig)
      JavaReflectMethod(m, sourceLines)
    }

object JavaReflectClass:
  def apply(cls: Class[?], sourceLineMap: Map[MethodSig, Seq[Int]]): JavaReflectClass =
    new JavaReflectClass(cls, sourceLineMap)

  def apply(cls: Class[?]): JavaReflectClass = new JavaReflectClass(cls, Map.empty)
