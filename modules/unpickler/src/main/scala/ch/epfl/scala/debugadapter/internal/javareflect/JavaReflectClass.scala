package ch.epfl.scala.debugadapter.internal.javareflect

import ch.epfl.scala.debugadapter.internal.binary
import scala.util.matching.Regex

class JavaReflectClass(cls: Class[?]) extends binary.ClassType:
  override def name: String = cls.getTypeName

  override def toString: String = cls.toString
