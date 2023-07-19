package ch.epfl.scala.debugadapter.internal.javareflect

import ch.epfl.scala.debugadapter.internal.binary

import java.lang.reflect.Parameter

class JavaReflectParameter(parameter: Parameter) extends binary.Parameter:

  override def name: String = parameter.getName

  override def `type`: binary.Type = JavaReflectType(parameter.getType)
