package ch.epfl.scala.debugadapter.internal.stacktrace

import tastyquery.Symbols.*
import tastyquery.Types.Type
import tastyquery.Types.TypeOrMethodic
import tastyquery.Contexts.Context

sealed trait BinarySymbol

enum BinaryClassSymbol(val symbol: Symbol, val kind: BinaryClassKind) extends BinarySymbol:
  case BinaryClass(override val symbol: ClassSymbol, override val kind: BinaryClassKind)
      extends BinaryClassSymbol(symbol, kind)
  case BinarySAMClass(term: TermSymbol, parentClass: ClassSymbol, tpe: Type)
      extends BinaryClassSymbol(term, BinaryClassKind.SAMClass)
  case BinaryPartialFunction(term: TermSymbol, tpe: Type)
      extends BinaryClassSymbol(term, BinaryClassKind.PartialFunction)

  override def toString: String =
    val span = symbol.tree.map(tree => s"(${tree.pos.startLine}, ${tree.pos.endLine})").getOrElse("")
    this match
      case BinarySAMClass(symbol, _, tpe) =>
        s"BinarySAMClass($symbol $span, ${tpe.showBasic})"
      case BinaryClass(symbol, kind) => s"BinaryClass($symbol $span, $kind)"
      case BinaryPartialFunction(symbol, tpe) =>
        s"BinaryPartialFunction($symbol $span, ${tpe.showBasic})"

enum BinaryClassKind:
  case TopLevelOrInner, Local, Anon, SAMClass, SyntheticCompanionClass, PartialFunction

enum BinaryMethodSymbol extends BinarySymbol:
  case BinaryMethod(binaryOwner: BinaryClassSymbol, term: TermSymbol, kind: BinaryMethodKind)
  case BinaryOuter(binaryOwner: BinaryClassSymbol, outerClass: ClassSymbol)
  case BinarySuperArg(binaryOwner: BinaryClassSymbol, init: TermSymbol, tpe: Type)
  case BinaryLiftedTry(binaryOwner: BinaryClassSymbol, tpe: Type)
  case BinaryByNameArg(binaryOwner: BinaryClassSymbol, tpe: Type, isAdapted: Boolean)
  case BinaryMethodBridge(target: BinaryMethodSymbol, tpe: TypeOrMethodic)
  case BinaryAnonOverride(binaryOwner: BinaryClassSymbol, overriddenMethod: TermSymbol, tpe: TypeOrMethodic)
  case BinaryStaticForwarder(binaryOwner: BinaryClassSymbol, target: BinaryMethod)

  def symbol = this match
    case BinaryMethod(_, term, _) => Some(term)
    case _ => None

  def symbolKind = this match
    case BinaryMethod(_, _, kind) => kind
    case _: BinaryOuter => BinaryMethodKind.Outer
    case _: BinarySuperArg => BinaryMethodKind.SuperArg
    case _: BinaryLiftedTry => BinaryMethodKind.LiftedTry
    case BinaryByNameArg(_, _, adapted) =>
      if adapted then BinaryMethodKind.AdaptedByNameArg else BinaryMethodKind.ByNameArg
    case _: BinaryMethodBridge => BinaryMethodKind.Bridge
    case _: BinaryAnonOverride => BinaryMethodKind.AnonOverride
    case _: BinaryStaticForwarder => BinaryMethodKind.StaticForwarder

enum BinaryMethodKind:
  case InstanceDef, LocalDef, AnonFun, AdaptedAnonFun, Getter, Setter, LazyInit, LocalLazyInit, Constructor,
    TraitConstructor, MixinForwarder, TraitStaticForwarder, SuperAccessor, DefaultParameter, Outer, SuperArg,
    TraitParamGetter, TraitParamSetter, LiftedTry, ByNameArg, AdaptedByNameArg, Bridge, AnonOverride, StaticForwarder
