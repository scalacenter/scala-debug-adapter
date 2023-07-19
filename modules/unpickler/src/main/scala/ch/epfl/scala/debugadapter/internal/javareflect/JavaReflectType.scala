package ch.epfl.scala.debugadapter.internal.javareflect

import ch.epfl.scala.debugadapter.internal.binary

class JavaReflectType(cls: Class[?]) extends binary.Type:
  override def name: String = cls.getTypeName
