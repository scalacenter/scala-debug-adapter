package ch.epfl.scala.debugadapter.internal.stacktrace

import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Names.*
import tastyquery.Types.*
import tastyquery.Modifiers.*
import ch.epfl.scala.debugadapter.internal.binary
import tastyquery.SourcePosition
import tastyquery.Contexts.*
import tastyquery.Signatures.*
import scala.util.control.NonFatal
import tastyquery.SourceLanguage

extension (symbol: Symbol)
  def isTrait = symbol.isClass && symbol.asClass.isTrait
  def isAnonFun = symbol.nameStr == "$anonfun"
  def isAnonClass = symbol.nameStr == "$anon"
  def isLocal = symbol.owner.isTerm
  def isModuleClass = symbol.isClass && symbol.asClass.isModuleClass
  def nameStr = symbol.name.toString
  def pos: SourcePosition = symbol.tree.map(_.pos).getOrElse(SourcePosition.NoPosition)
  def isInline = symbol.isTerm && symbol.asTerm.isInline

extension (symbol: TermSymbol)
  private def isGetter = !symbol.isMethod
  private def isModuleOrLazyVal: Boolean = symbol.isLazyVal || symbol.isModuleVal
  private def isLazyVal: Boolean = symbol.kind == TermSymbolKind.LazyVal
  private def isVal: Boolean = symbol.kind == TermSymbolKind.Val
  def declaredTypeAsSeenFrom(tpe: Type)(using Context): TypeOrMethodic =
    symbol.declaredType.asSeenFrom(tpe, symbol.owner)
  def targetNameStr(using Context): String = symbol.targetName.toString
  def isOverridingSymbol(siteClass: ClassSymbol)(using Context): Boolean =
    val overridingSymbol =
      siteClass.linearization.iterator.flatMap(inClass => symbol.matchingSymbol(inClass, siteClass)).next
    overridingSymbol == symbol
  def isJavaOverride(using Context): Boolean =
    symbol.allOverriddenSymbols.exists(_.sourceLanguage == SourceLanguage.Java)

extension [A](xs: Seq[A])
  def singleOrElse[B >: A](x: => B): B =
    if xs.size == 1 then xs.head else x

  def orIfEmpty[B >: A](ys: => Seq[B]): Seq[B] =
    if xs.nonEmpty then xs else ys

extension [T <: BinarySymbol](xs: Seq[T])
  def singleOrThrow(symbol: binary.Symbol): T =
    singleOptOrThrow(symbol)
      .getOrElse(throw new NotFoundException(symbol))

  def singleOptOrThrow(symbol: binary.Symbol): Option[T] =
    if xs.size > 1 then throw new AmbiguousException(symbol, xs)
    else xs.headOption

extension (name: Name)
  def isPackageObject: Boolean =
    val nameStr = name.toString
    nameStr == "package" || nameStr.endsWith("$package")

extension (tpe: TermType) def isMethodic: Boolean = tpe.isInstanceOf[MethodicType]

extension (tpe: TypeOrMethodic)
  def allParamTypes: Seq[Type] = tpe match
    case t: MethodType => t.paramTypes ++ t.resultType.allParamTypes
    case t: PolyType => t.resultType.allParamTypes
    case _ => Seq.empty

  def allParamNames: Seq[TermName] = tpe match
    case t: MethodType => t.paramNames ++ t.resultType.allParamNames
    case t: PolyType => t.resultType.allParamNames
    case _ => Seq.empty

  def returnType: Type = tpe match
    case t: (MethodType | PolyType) => t.resultType.returnType
    case t: Type => t

  def isByName: Boolean = tpe.isInstanceOf[ByNameType]

extension (tpe: Type)
  def isFunction: Boolean =
    tpe match
      case ref: TypeRef => ref.prefix.isScalaPackage && ref.nameStr.startsWith("Function")
      case _ => false

  def isContextFunction: Boolean =
    tpe match
      case ref: TypeRef => ref.prefix.isScalaPackage && ref.nameStr.startsWith("ContextFunction")
      case _ => false

  def isTuple: Boolean =
    tpe match
      case ref: TypeRef =>
        isScalaPackage(ref.prefix) && ref.nameStr.startsWith("Tuple")
      case _ => false

  def isRepeatedParam(using Context): Boolean =
    tpe match
      case ref: TypeRef => ref.optSymbol.exists(_ == ctx.defn.RepeatedParamClass)
      case _ => false

  def isOperatorLike: Boolean =
    tpe match
      case ref: TypeRef =>
        val operatorChars = "\\+\\-\\*\\/\\%\\&\\|\\^\\<\\>\\=\\!\\~\\#\\:\\@\\?"
        val regex = s"[^$operatorChars]".r
        !regex.findFirstIn(ref.nameStr).isDefined
      case _ => false

extension (tpe: NamedType) def nameStr: String = tpe.name.toString

extension (tpe: TypeOrWildcard)
  def erasedAsReturnType(using Context): FullyQualifiedName = erased(isReturnType = true)
  def erasedAsArgType(asJavaVarargs: Boolean = false)(using Context): FullyQualifiedName =
    tpe match
      case t: AppliedType if t.tycon.isRepeatedParam && asJavaVarargs =>
        AppliedType(TypeRef(ctx.defn.scalaPackage.packageRef, ctx.defn.ArrayClass), t.args).erased(isReturnType = false)
      case _ => tpe.erased(isReturnType = false)

  private def erased(isReturnType: Boolean)(using Context): FullyQualifiedName =
    tpe match
      case tpe: Type => ErasedTypeRef.erase(tpe, SourceLanguage.Scala3, keepUnit = isReturnType).toSigFullName
      case _: WildcardTypeArg => ctx.defn.ObjectClass.fullName

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
