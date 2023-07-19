package ch.epfl.scala.debugadapter.internal.binary

trait Method:
  def name: String
  def declaringClass: ClassType
  def parameters: Seq[Parameter]
  // return None if the class of the return type is not yet loaded
  def returnType: Option[Type]
  def returnTypeName: String

  def isExtensionMethod: Boolean = name.endsWith("$extension")
  def isTraitInitializer: Boolean = name == "$init$"
  def isClassInitializer: Boolean = name == "<init>"
