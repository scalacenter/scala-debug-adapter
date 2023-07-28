package ch.epfl.scala.debugadapter.internal.javareflect

import ch.epfl.scala.debugadapter.internal.binary
import scala.util.matching.Regex
import scala.jdk.CollectionConverters.*

class JavaReflectClass(cls: Class[?]) extends binary.ClassType:

  override def name: String = cls.getTypeName

  override def superclass = Option(cls.getSuperclass).map(JavaReflectClass(_))

  override def interfaces = cls.getInterfaces.toList.map(JavaReflectClass(_))

  override def toString: String = cls.toString

  override def isInterface: Boolean = cls.isInterface()

  def declaredMethods: Seq[binary.Method] =
    cls.getDeclaredMethods.map(JavaReflectMethod(_)) ++
      cls.getDeclaredConstructors.map(JavaReflectConstructor(_))
