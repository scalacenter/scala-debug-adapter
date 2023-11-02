package ch.epfl.scala.debugadapter.internal.javareflect

import ch.epfl.scala.debugadapter.internal.binary
import scala.util.matching.Regex
import scala.jdk.CollectionConverters.*
import ch.epfl.scala.debugadapter.internal.binary.Method
import ch.epfl.scala.debugadapter.internal.binary.MethodSig
import ch.epfl.scala.debugadapter.internal.binary.SourceLines

class JavaReflectClass(cls: Class[?], extraInfo: ExtraClassInfo, loader: JavaReflectLoader) extends binary.ClassType:
  override def name: String = cls.getTypeName
  override def superclass = Option(cls.getSuperclass).map(loader.loadClass)
  override def interfaces = cls.getInterfaces.toList.map(loader.loadClass)
  override def isInterface: Boolean = cls.isInterface
  override def sourceLines: Option[SourceLines] = extraInfo.sourceLines

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

  override def toString: String =
    if showSpan.isEmpty then cls.toString else s"$cls $showSpan"

  override def declaredMethods: Seq[binary.Method] =
    cls.getDeclaredMethods.map { m =>
      val sig = JavaReflectUtils.signature(m)
      val methodInfo = extraInfo.getMethodInfo(sig)
      JavaReflectMethod(m, sig, methodInfo, loader)
    } ++ cls.getDeclaredConstructors.map { c =>
      val sig = JavaReflectUtils.signature(c)
      val methodInfo = extraInfo.getMethodInfo(sig)
      JavaReflectConstructor(c, sig, methodInfo, loader)
    }
