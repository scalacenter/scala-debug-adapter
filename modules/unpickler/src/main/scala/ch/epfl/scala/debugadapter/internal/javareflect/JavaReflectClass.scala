package ch.epfl.scala.debugadapter.internal.javareflect

import ch.epfl.scala.debugadapter.internal.binary
import scala.util.matching.Regex
import scala.jdk.CollectionConverters.*
import ch.epfl.scala.debugadapter.internal.binary.Method
import ch.epfl.scala.debugadapter.internal.binary.MethodSig

class JavaReflectClass(cls: Class[?], extraInfos: Map[MethodSig, ExtraBytecodeInfo], loader: JavaReflectLoader)
    extends binary.ClassType:
  override def name: String = cls.getTypeName
  override def superclass = Option(cls.getSuperclass).map(loader.loadClass)
  override def interfaces = cls.getInterfaces.toList.map(loader.loadClass)
  override def isInterface: Boolean = cls.isInterface
  override def sourceLines: Seq[binary.SourceLine] =
    extraInfos.values.map(_.sourceLines).flatten.toSeq.distinct.sorted

  override def declaredMethod(name: String, sig: String): Option[Method] =
    declaredMethods.find(m => m.signature == MethodSig(name, sig))

  override def method(name: String, sig: String): Option[Method] =
    declaredMethod(name, sig).orElse {
      for
        method <- cls.getMethods.find(m => JavaReflectUtils.signature(m) == MethodSig(name, sig))
        declaringClass = loader.loadClass(method.getDeclaringClass)
        declaredMethod <- declaringClass.declaredMethod(name, sig)
      yield declaredMethod
    }

  override def toString: String = cls.toString

  override def declaredMethods: Seq[binary.Method] =
    cls.getDeclaredMethods.map { m =>
      val sig = JavaReflectUtils.signature(m)
      val extraInfo = extraInfos.getOrElse(sig, ExtraBytecodeInfo.empty)
      JavaReflectMethod(m, sig, extraInfo, loader)
    } ++ cls.getDeclaredConstructors.map { c =>
      val sig = JavaReflectUtils.signature(c)
      val extraInfo = extraInfos.getOrElse(sig, ExtraBytecodeInfo.empty)
      JavaReflectConstructor(c, sig, extraInfo, loader)
    }
