package ch.epfl.scala.debugadapter.internal.javareflect

import ch.epfl.scala.debugadapter.internal.binary
import scala.util.matching.Regex
import scala.jdk.CollectionConverters.*

class JavaReflectClass(cls: Class[?], sourceLineMap: Map[MethodSig, Seq[binary.SourceLine]], loader: JavaReflectLoader)
    extends binary.ClassType:
  override def name: String = cls.getTypeName
  override def superclass = Option(cls.getSuperclass).map(loader.loadClass)
  override def interfaces = cls.getInterfaces.toList.map(loader.loadClass)
  override def isInterface: Boolean = cls.isInterface
  override def sourceLines: Seq[binary.SourceLine] =
    sourceLineMap.values.flatten.toSeq.distinct.sorted

  override def toString: String = cls.toString

  def declaredMethodsAndConstructors: Seq[binary.Method] =
    declaredConstructors ++ declaredMethods

  private def declaredConstructors: Seq[binary.Method] =
    cls.getDeclaredConstructors.map { c =>
      val sig = JavaReflectUtils.signature(c)
      val sourceLines = sourceLineMap.getOrElse(sig, Seq.empty)
      JavaReflectConstructor(c, sourceLines, loader)
    }

  private def declaredMethods: Seq[binary.Method] =
    cls.getDeclaredMethods.map { m =>
      val sig = JavaReflectUtils.signature(m)
      val sourceLines = sourceLineMap.getOrElse(sig, Seq.empty)
      JavaReflectMethod(m, sourceLines, loader)
    }
