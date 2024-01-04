package ch.epfl.scala.debugadapter.internal.javareflect

import ch.epfl.scala.debugadapter.internal.binary
import scala.util.matching.Regex
import scala.jdk.CollectionConverters.*

class JavaReflectClass(cls: Class[?], extraInfo: ExtraClassInfo, override val classLoader: JavaReflectLoader)
    extends binary.ClassType:
  override def name: String = cls.getTypeName
  override def superclass = Option(cls.getSuperclass).map(classLoader.loadClass)
  override def interfaces = cls.getInterfaces.toList.map(classLoader.loadClass)
  override def isInterface: Boolean = cls.isInterface
  override def sourceLines: Option[binary.SourceLines] = extraInfo.sourceLines

  override def declaredMethod(name: String, sig: String): Option[binary.Method] =
    declaredMethods.find(m => m.signedName == binary.SignedName(name, sig))

  override def method(name: String, sig: String): Option[binary.Method] =
    declaredMethod(name, sig).orElse {
      for
        method <- cls.getMethods.find(m => JavaReflectUtils.signature(m) == binary.SignedName(name, sig))
        declaringClass = classLoader.loadClass(method.getDeclaringClass)
        declaredMethod <- declaringClass.declaredMethod(name, sig)
      yield declaredMethod
    }

  override def declaredField(name: String): Option[binary.Field] =
    try Some(JavaReflectField(cls.getDeclaredField(name), classLoader))
    catch case _: NoSuchFieldException => None

  override def toString: String =
    if showSpan.isEmpty then cls.toString else s"$cls $showSpan"

  override def declaredMethods: Seq[binary.Method] =
    cls.getDeclaredMethods.map { m =>
      val sig = JavaReflectUtils.signature(m)
      val methodInfo = extraInfo.getMethodInfo(sig)
      JavaReflectMethod(m, sig, methodInfo, classLoader)
    } ++ cls.getDeclaredConstructors.map { c =>
      val sig = JavaReflectUtils.signature(c)
      val methodInfo = extraInfo.getMethodInfo(sig)
      JavaReflectConstructor(c, sig, methodInfo, classLoader)
    }
