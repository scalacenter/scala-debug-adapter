package ch.epfl.scala.debugadapter.internal.binary

trait Method extends Symbol:
  def declaringClass: ClassType
  def allParameters: Seq[Parameter]
  // return None if the class of the return type is not yet loaded
  def returnType: Option[Type]
  def returnTypeName: String
  def isBridge: Boolean
  def isStatic: Boolean
  def instructions: Seq[Instruction]
  def isConstructor: Boolean
  def signature: MethodSig

  def isExtensionMethod: Boolean = name.endsWith("$extension") && !isStatic
  def isTraitStaticForwarder: Boolean =
    name.endsWith("$") && !isDeserializeLambda && !isTraitInitializer && declaringClass.isInterface && isStatic
  def isTraitInitializer: Boolean = name == "$init$" && isStatic
  def isClassInitializer: Boolean = name == "<init>"
  def isPartialFunctionApplyOrElse: Boolean = declaringClass.isPartialFunction && name == "applyOrElse"
  def isDeserializeLambda: Boolean =
    isStatic &&
      name == "$deserializeLambda$" &&
      allParameters.map(_.`type`.name) == Seq("java.lang.invoke.SerializedLambda")
  def isAnonFun: Boolean = name.matches("(.*)\\$anonfun\\$\\d+")
