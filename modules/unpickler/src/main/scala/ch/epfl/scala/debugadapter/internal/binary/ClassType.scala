package ch.epfl.scala.debugadapter.internal.binary
trait ClassType extends Type:
  def name: String
  def isInterface: Boolean
  def superclass: Option[ClassType]
  def interfaces: Seq[ClassType]
  def method(name: String, descriptor: String): Option[Method]
  def declaredField(name: String): Option[Field]
  def declaredMethod(name: String, descriptor: String): Option[Method]
  def declaredMethods: Seq[Method]

  def isObject = name.endsWith("$")
  def isPackageObject = name.endsWith(".package$") || name.endsWith("$package$")
  def isPartialFunction = superclass.exists(_.name == "scala.runtime.AbstractPartialFunction")
  def isJavaLangEnum = superclass.exists(_.name == "java.lang.Enum")
