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
  def isLocal = symbol.owner.isTerm
  def isModuleClass = symbol.isClass && symbol.asClass.isModuleClass

  def pos: SourcePosition = symbol.tree.map(_.pos).getOrElse(SourcePosition.NoPosition)
  def isInline = symbol.isTerm && symbol.asTerm.isInline

extension (symbol: ClassSymbol)
  def isAnonClass = symbol.name == CommonNames.anonClass
  def nameStr: String = symbol.name.toString()

  /** The name of this class in the source, i.e., without the trailing `$` for module classes. */
  def sourceName: String = symbol.name match
    case ObjectClassTypeName(underlying) => underlying.toString()
    case name => name.toString()

extension (symbol: TermSymbol)
  private def isGetter = !symbol.isMethod
  private def isModuleOrLazyVal: Boolean = symbol.isLazyVal || symbol.isModuleVal
  private def isLazyVal: Boolean = symbol.kind == TermSymbolKind.LazyVal
  private def isVal: Boolean = symbol.kind == TermSymbolKind.Val
  def nameStr: String = symbol.name.toString()
  def isAnonFun = symbol.name == CommonNames.anonFun
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

extension (name: TermName)
  def isPackageObject: Boolean =
    val nameStr = name.toString
    nameStr == "package" || nameStr.endsWith("$package")

extension (name: ClassTypeName)
  def isPackageObjectClass: Boolean = name match
    case SimpleTypeName(_) => false
    case ObjectClassTypeName(underlying) => underlying.toTermName.isPackageObject

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

extension (tpe: Type)
  private def isNumberedTypeRefInScalaPackage(namePrefix: String): Boolean =
    tpe match
      case ref: TypeRef =>
        ref.prefix.isScalaPackage &&
        ref.name.match
          case SimpleTypeName(nameStr) => nameStr.startsWith(namePrefix)
          case _: ObjectClassTypeName | _: UniqueTypeName => false
      case _ =>
        false

  def isFunction: Boolean = isNumberedTypeRefInScalaPackage("Function")

  def isContextFunction: Boolean = isNumberedTypeRefInScalaPackage("ContextFunction")

  def isTuple: Boolean = isNumberedTypeRefInScalaPackage("Tuple")

  def isOperatorLike: Boolean =
    tpe match
      case ref: TypeRef =>
        val operatorChars = "\\+\\-\\*\\/\\%\\&\\|\\^\\<\\>\\=\\!\\~\\#\\:\\@\\?"
        val regex = s"[^$operatorChars]".r
        !regex.findFirstIn(ref.name.toString()).isDefined
      case _ => false

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
    isScalaPackage(ref.prefix) && ref.name == CommonNames.Predef

extension (prefix: Prefix)
  def isScalaPackage: Boolean =
    prefix match
      case p: PackageRef => p.symbol.name == nme.scalaPackageName && p.symbol.owner.isRootPackage
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
