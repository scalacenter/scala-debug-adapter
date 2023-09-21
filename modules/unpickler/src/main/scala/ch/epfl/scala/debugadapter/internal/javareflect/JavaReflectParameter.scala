package ch.epfl.scala.debugadapter.internal.javareflect

import ch.epfl.scala.debugadapter.internal.binary

import java.lang.reflect.Parameter

class JavaReflectParameter(parameter: Parameter) extends binary.Parameter:

  override def name: String = parameter.getName
  override def sourceLines: Seq[Int] = Seq.empty

  override def `type`: binary.Type = JavaReflectClass(parameter.getType)

  override def toString: String = parameter.toString
