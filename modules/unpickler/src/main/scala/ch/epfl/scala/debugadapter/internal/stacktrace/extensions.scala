package ch.epfl.scala.debugadapter.internal.stacktrace

import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Names.*
import tastyquery.Types.*
import tastyquery.Modifiers.*
import ch.epfl.scala.debugadapter.internal.binary
import tastyquery.SourcePosition

extension (symbol: Symbol)
  def isTrait = symbol.isClass && symbol.asClass.isTrait
  def isAnonFun = symbol.nameStr == "$anonfun"
  def isAnonClass = symbol.nameStr == "$anon"
  def matchName(name: String) = symbol.nameStr == name
  def isLocal = symbol.owner.isTerm
  def isModuleClass = symbol.isClass && symbol.asClass.isModuleClass
  def nameStr = symbol.name.toString
  def pos: SourcePosition = symbol.tree.map(_.pos).getOrElse(SourcePosition.NoPosition)

extension (symbol: TermSymbol)
  private def isGetterOrSetter = !symbol.isMethod || symbol.isSetter
  private def isLazyValInTrait: Boolean = symbol.owner.isTrait && symbol.isLazyVal
  private def isLazyVal: Boolean = symbol.kind == TermSymbolKind.LazyVal

extension [T <: BinarySymbol](candidates: Seq[T])
  def singleOrThrow(symbol: binary.Symbol): T =
    singleOptOrThrow(symbol)
      .getOrElse(throw new NotFoundException(symbol))

  def singleOptOrThrow(symbol: binary.Symbol): Option[T] =
    if candidates.size > 1 then throw new AmbiguousException(symbol, candidates)
    else candidates.headOption

extension (name: Name)
  def isPackageObject: Boolean =
    val nameStr = name.toString
    nameStr == "package" || nameStr.endsWith("$package")

extension (tpe: TypeOrMethodic)
  def allParamsTypes: Seq[Type] = tpe match
    case t: MethodType => t.paramTypes ++ t.resultType.allParamsTypes
    case t: PolyType => t.resultType.allParamsTypes
    case _ => Seq.empty

  def allParamsNames: Seq[TermName] = tpe match
    case t: MethodType => t.paramNames ++ t.resultType.allParamsNames
    case t: PolyType => t.resultType.allParamsNames
    case _ => Seq.empty

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

extension (tree: Apply)
  def allArgsFlatten: Seq[TermTree] =
    def rec(fun: Tree): Seq[TermTree] = fun match
      case TypeApply(fun, _) => rec(fun)
      case Apply(fun, args) => rec(fun) ++ args
      case _ => Seq.empty
    rec(tree.fun) ++ tree.args

extension (pos: SourcePosition)
  def isEnclosing(other: SourcePosition): Boolean =
    pos.sourceFile == other.sourceFile
      && pos.hasLineColumnInformation
      && other.hasLineColumnInformation
      && pos.startLine < other.startLine
      && pos.endLine > other.endLine

  def containsLine(sourceLine: binary.SourceLine): Boolean =
    !pos.isUnknown
      && pos.hasLineColumnInformation
      && pos.startLine <= sourceLine.toTasty
      && pos.endLine >= sourceLine.toTasty

  def unknownOrContainsAll(span: Seq[binary.SourceLine]): Boolean =
    pos.isUnknown
      || !pos.hasLineColumnInformation
      || span.forall(pos.containsLine)

extension (sourceLines: Seq[binary.SourceLine])
  def interval: Seq[binary.SourceLine] =
    if sourceLines.size > 2 then Seq(sourceLines.min, sourceLines.max)
    else sourceLines
