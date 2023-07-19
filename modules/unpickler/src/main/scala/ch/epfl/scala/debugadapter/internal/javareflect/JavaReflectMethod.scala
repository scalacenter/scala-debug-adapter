package ch.epfl.scala.debugadapter.internal.javareflect

import ch.epfl.scala.debugadapter.internal.binary

import java.lang.reflect.Method

class JavaReflectMethod(method: Method) extends binary.Method:

  override def returnType: Option[binary.Type] =
    Option(method.getReturnType).map(JavaReflectType.apply(_))

  override def returnTypeName: String = method.getReturnType.getName

  override def declaringType: binary.ReferenceType =
    JavaReflectReferencType(method.getDeclaringClass)

  override def parameters: Seq[binary.Parameter] =
    method.getParameters.map(JavaReflectParameter.apply(_))

  override def name: String = method.getName
