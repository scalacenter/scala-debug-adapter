package ch.epfl.scala.debugadapter.internal.javareflect

import ch.epfl.scala.debugadapter.internal.binary

import java.lang.reflect.Constructor
import java.lang.reflect.Method

class JavaReflectConstructor(constructor: Constructor[?]) extends binary.Method:

  override def returnType: Option[binary.Type] =
    Some(JavaReflectClass(classOf[Unit]))

  override def returnTypeName: String = "void"

  override def declaringClass: binary.ClassType =
    JavaReflectClass(constructor.getDeclaringClass)

  override def parameters: Seq[binary.Parameter] =
    constructor.getParameters.map(JavaReflectParameter.apply(_))

  override def name: String = "<init>"
