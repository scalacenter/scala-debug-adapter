package ch.epfl.scala.debugadapter.internal.stacktrace

import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Names.*
import tastyquery.Types.*
import tastyquery.Modifiers.*

extension (symbol: Symbol)
  def isTrait = symbol.isClass && symbol.asClass.isTrait
  def isAnonFun = symbol.nameStr == "$anonfun"
  def isAnonClass = symbol.nameStr == "$anon"
  def matchName(name: String) =
    symbol.nameStr == name
  def isLocal = symbol.owner.isTerm
  def isModuleClass = symbol.isClass && symbol.asClass.isModuleClass
  def nameStr = symbol.name.toString

extension (symbol: TermSymbol)
  private def isGetterOrSetter = !symbol.isMethod || symbol.isSetter
  private def isLazyValInTrait: Boolean = symbol.owner.isTrait && symbol.isLazyVal
  private def isLazyVal: Boolean = symbol.kind == TermSymbolKind.LazyVal

extension [T](binarySymbols: Seq[T])
  def singleOrThrow(binaryName: String): T =
    singleOptOrThrow(binaryName)
      .getOrElse(throw new NotFoundException(s"Cannot find Scala symbol of $binaryName"))

  def singleOptOrThrow(binaryName: String): Option[T] =
    if binarySymbols.size > 1 then
      throw new AmbiguousException(s"Found ${binarySymbols.size} matching symbols for $binaryName")
    else binarySymbols.headOption

extension (name: Name)
  def isPackageObject: Boolean =
    val nameStr = name.toString
    nameStr == "package" || nameStr.endsWith("$package")

extension (tpe: Type)
  def isFunction: Boolean =
    tpe match
      case ref: TypeRef =>
        isScalaPackage(ref.prefix) && ref.nameStr.startsWith("Function")
      case _ => false

  def isTuple: Boolean =
    tpe match
      case ref: TypeRef =>
        isScalaPackage(ref.prefix) && ref.nameStr.startsWith("Tuple")
      case _ => false
  def isVarArg: Boolean =
    tpe match
      case ref: TypeRef =>
        isScalaPackage(ref.prefix) && ref.nameStr == "<repeated>"
      case _ => false

  def isOperatorLike: Boolean =
    tpe match
      case ref: TypeRef =>
        val operatorChars = "\\+\\-\\*\\/\\%\\&\\|\\^\\<\\>\\=\\!\\~\\#\\:\\@\\?"
        val regex = s"[^$operatorChars]".r
        !regex.findFirstIn(ref.nameStr).isDefined
      case _ => false

extension (tpe: NamedType) def nameStr: String = tpe.name.toString

extension (ref: TermRef)
  def isScalaPredef: Boolean =
    isScalaPackage(ref.prefix) && ref.nameStr == "Predef"

extension (prefix: Prefix)
  def isScalaPackage: Boolean =
    prefix match
      case p: PackageRef => p.fullyQualifiedName.toString == "scala"
      case _ => false
