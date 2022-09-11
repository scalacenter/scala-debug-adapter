package ch.epfl.scala.debugadapter.internal.jdi

import scala.jdk.CollectionConverters.*

class Method(val obj: Any) extends JavaReflection:
  def name: String = invokeMethod("name")

  def declaringType: ReferenceType =
    ReferenceType(invokeMethod("declaringType"))

  def arguments: Seq[LocalVariable] =
    invokeMethod[java.util.List[Object]]("arguments").asScala.toSeq
      .map(LocalVariable.apply(_))

  def returnType: Type =
    Type(invokeMethod("returnType"))

  def signature: String = invokeMethod("signature")

  override def toString: String = invokeMethod("toString")
