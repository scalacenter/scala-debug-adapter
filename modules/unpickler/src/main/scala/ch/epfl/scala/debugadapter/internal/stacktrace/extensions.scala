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

  def nameStr = symbol.name match
    case ObjectClassName(underlying) => underlying.toString()
    case ObjectClassTypeName(underlying) => underlying.toString()
    case name => name.toString()

  def pos: SourcePosition = symbol.tree.map(_.pos).getOrElse(SourcePosition.NoPosition)
  def isInline = symbol.isTerm && symbol.asTerm.isInline

extension (symbol: TermSymbol)
  private def isGetter = !symbol.isMethod
  private def isModuleOrLazyVal: Boolean = symbol.isLazyVal || symbol.isModuleVal
  private def isLazyVal: Boolean = symbol.kind == TermSymbolKind.LazyVal
  private def isVal: Boolean = symbol.kind == TermSymbolKind.Val
  def targetNameStr(using Context): String = symbol.targetName.toString
  def overridingSymbolInLinearization(siteClass: ClassSymbol)(using Context): TermSymbol =
    siteClass.linearization.iterator.flatMap(inClass => symbol.matchingSymbol(inClass, siteClass)).next
  def isOverridingSymbol(siteClass: ClassSymbol)(using Context): Boolean =
    overridingSymbolInLinearization(siteClass) == symbol

extension [A, S[+X] <: IterableOnce[X]](xs: S[A])
  def singleOpt: Option[A] =
    Option.when(xs.size == 1)(xs.iterator.next)

  def singleOrElse[B >: A](x: => B): B =
    if xs.size == 1 then xs.iterator.next else x

  def orIfEmpty[B >: A](ys: => S[B]): S[B] =
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
    val nameStr = name.toString.stripSuffix("$")
    nameStr == "package" || nameStr.endsWith("$package")

extension (tpe: TermType) def isMethodic: Boolean = tpe.isInstanceOf[MethodicType]

extension (tpe: TypeOrMethodic)
  def allParamTypes: List[Type] = tpe match
    case t: MethodType => t.paramTypes ++ t.resultType.allParamTypes
    case t: PolyType => t.resultType.allParamTypes
    case _ => Nil

  def allParamNames: List[UnsignedTermName] = tpe match
    case t: MethodType => t.paramNames ++ t.resultType.allParamNames
    case t: PolyType => t.resultType.allParamNames
    case _ => Nil

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

  def isOperatorLike: Boolean =
    tpe match
      case ref: TypeRef =>
        val operatorChars = "\\+\\-\\*\\/\\%\\&\\|\\^\\<\\>\\=\\!\\~\\#\\:\\@\\?"
        val regex = s"[^$operatorChars]".r
        !regex.findFirstIn(ref.nameStr).isDefined
      case _ => false

extension (tpe: NamedType) def nameStr: String = tpe.name.toString

extension (tpe: Type)
  def erasedAsReturnType(using Context): ErasedTypeRef = erased(isReturnType = true)
  def erasedAsArgType(asJavaVarargs: Boolean = false)(using Context): ErasedTypeRef =
    tpe match
      case tpe: RepeatedType if asJavaVarargs =>
        ctx.defn.ArrayTypeOf(tpe.elemType).erased(isReturnType = false)
      case _ => tpe.erased(isReturnType = false)

  private def erased(isReturnType: Boolean)(using Context): ErasedTypeRef =
    ErasedTypeRef.erase(tpe, SourceLanguage.Scala3, keepUnit = isReturnType)

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

  def containsLine(line: Int): Boolean =
    !pos.isUnknown
      && pos.hasLineColumnInformation
      && pos.startLine <= line
      && pos.endLine >= line

extension (binaryClass: BinaryClassSymbol)
  def isJava: Boolean =
    binaryClass match
      case BinaryClass(symbol) => symbol.sourceLanguage == SourceLanguage.Java
      case _ => false
