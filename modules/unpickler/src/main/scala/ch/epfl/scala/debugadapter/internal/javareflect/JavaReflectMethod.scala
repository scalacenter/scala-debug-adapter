package ch.epfl.scala.debugadapter.internal.javareflect

import ch.epfl.scala.debugadapter.internal.binary

import java.lang.reflect.Method

class JavaReflectMethod(method: Method, val sourceLines: Seq[Int]) extends binary.Method:

  override def returnType: Option[binary.Type] =
    Option(method.getReturnType).map(JavaReflectClass(_))

  override def returnTypeName: String = method.getReturnType.getName

  override def declaringClass: binary.ClassType =
    JavaReflectClass(method.getDeclaringClass)

  override def allParameters: Seq[binary.Parameter] =
    method.getParameters.map(JavaReflectParameter.apply(_))

  override def name: String = method.getName

  override def toString: String = method.toString
