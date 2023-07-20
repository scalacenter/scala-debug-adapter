package ch.epfl.scala.debugadapter.internal.binary

trait Method:
  def name: String
  def declaringClass: ClassType
  def allParameters: Seq[Parameter]
  // return None if the class of the return type is not yet loaded
  def returnType: Option[Type]
  def returnTypeName: String

  def isExtensionMethod: Boolean = name.endsWith("$extension")
  def isTraitInitializer: Boolean = name == "$init$"
  def isClassInitializer: Boolean = name == "<init>"
  def isLocalMethod: Boolean = name.matches(".*\\$\\d+")
  def declaredParams: Seq[Parameter] =
    if isExtensionMethod && allParameters.headOption.exists(_.isThis) then allParameters.tail
    else if isClassInitializer && allParameters.headOption.exists(_.isOuter) then allParameters.tail
    else if isLocalMethod then allParameters.dropWhile(_.isCapture)
    else allParameters
  def declaredReturnType: Option[Type] =
    if isClassInitializer then Some(declaringClass) else returnType
