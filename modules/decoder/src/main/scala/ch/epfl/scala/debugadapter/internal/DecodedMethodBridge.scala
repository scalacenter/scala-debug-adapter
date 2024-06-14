package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.decoder.DecodedMethod
import ch.epfl.scala.decoder.StackTraceFormatter
import tastyquery.Symbols.*
import tastyquery.Modifiers.*

class DecodedMethodBridge(method: DecodedMethod, formatter: StackTraceFormatter):
  def format: String = formatter.format(method)
  def isGenerated: Boolean = method.isGenerated

  extension (method: DecodedMethod)
    private def isGenerated: Boolean =
      method match
        case method: DecodedMethod.ValOrDefDef =>
          val sym = method.symbol
          (sym.isGetter && (!sym.owner.isTrait || !sym.isModuleOrLazyVal)) || // getter
          (sym.isLocal && sym.isModuleOrLazyVal) || // local def
          sym.isSetter ||
          (sym.isSynthetic && !sym.isLocal) ||
          sym.isExport
        case method: DecodedMethod.LazyInit => method.symbol.owner.isTrait
        case _: DecodedMethod.TraitStaticForwarder => true
        case _: DecodedMethod.TraitParamAccessor => true
        case _: DecodedMethod.MixinForwarder => true
        case _: DecodedMethod.Bridge => true
        case _: DecodedMethod.StaticForwarder => true
        case _: DecodedMethod.OuterAccessor => true
        case _: DecodedMethod.SetterAccessor => true
        case _: DecodedMethod.GetterAccessor => true
        case _: DecodedMethod.SuperAccessor => true
        case _: DecodedMethod.SpecializedMethod => true
        case _: DecodedMethod.InlineAccessor => true
        case _: DecodedMethod.AdaptedFun => true
        case _: DecodedMethod.SAMOrPartialFunctionConstructor => true
        case method: DecodedMethod.InlinedMethod => method.underlying.isGenerated
        case _ => false

  extension (symbol: TermSymbol)
    private def isGetter = !symbol.isMethod
    private def isModuleOrLazyVal: Boolean = symbol.isLazyVal || symbol.isModuleVal
    private def isLazyVal: Boolean = symbol.kind == TermSymbolKind.LazyVal

  extension (symbol: Symbol)
    private def isTrait = symbol.isClass && symbol.asClass.isTrait
    private def isLocal = symbol.owner.isTerm
