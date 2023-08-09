package ch.epfl.scala.debugadapter.internal.stacktrace

import tastyquery.Symbols.*
import tastyquery.Modifiers.TermSymbolKind

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

extension [T <: Symbol](symbols: Seq[T])
  def singleOrThrow(binaryName: String): T =
    singleOptOrThrow(binaryName)
      .getOrElse(throw new NotFoundException(s"Cannot find Scala symbol of $binaryName"))

  def singleOptOrThrow(binaryName: String): Option[T] =
    if symbols.size > 1 then throw new AmbiguousException(s"Found ${symbols.size} matching symbols for $binaryName")
    else symbols.headOption
