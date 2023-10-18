package ch.epfl.scala.debugadapter.internal.stacktrace

import tastyquery.Symbols.*
import tastyquery.Types.Type
import tastyquery.Types.TypeOrMethodic
import tastyquery.Contexts.Context

sealed trait BinarySymbol

sealed trait BinaryClassSymbol extends BinarySymbol:
  def symbol: Symbol
  override def toString: String =
    def format(sym: Symbol) =
      val span = sym.tree.map(tree => s"(${tree.pos.startLine}, ${tree.pos.endLine})").getOrElse("")
      s"$sym $span"
    this match
      case BinaryClass(sym) => s"BinaryClass(${format(sym)})"
      case BinarySyntheticCompanionClass(symbol) => s"BinarySyntheticCompanionClass(${format(symbol)})"
      case BinarySAMClass(sym, _, tpe) => s"BinarySAMClass(${format(sym)}, ${tpe.showBasic})"
      case BinaryPartialFunction(sym, tpe) => s"BinaryPartialFunction(${format(sym)}, ${tpe.showBasic})"

final case class BinaryClass(symbol: ClassSymbol) extends BinaryClassSymbol
final case class BinarySyntheticCompanionClass(symbol: ClassSymbol) extends BinaryClassSymbol
final case class BinarySAMClass(symbol: TermSymbol, parentClass: ClassSymbol, tpe: Type) extends BinaryClassSymbol
final case class BinaryPartialFunction(symbol: TermSymbol, tpe: Type) extends BinaryClassSymbol

enum BinaryMethodSymbol extends BinarySymbol:
  case BinaryMethod(binaryOwner: BinaryClassSymbol, term: TermSymbol, kind: BinaryMethodKind)
  case BinaryOuter(binaryOwner: BinaryClassSymbol, outerClass: ClassSymbol)
  case BinarySuperArg(binaryOwner: BinaryClassSymbol, init: TermSymbol, tpe: Type)
  case BinaryLiftedTry(binaryOwner: BinaryClassSymbol, tpe: Type)
  case BinaryByNameArg(binaryOwner: BinaryClassSymbol, tpe: Type, isAdapted: Boolean)
  case BinaryMethodBridge(target: BinaryMethodSymbol, tpe: TypeOrMethodic)
  case BinaryAnonOverride(binaryOwner: BinaryClassSymbol, overriddenMethod: TermSymbol, tpe: TypeOrMethodic)
  case BinaryStaticForwarder(binaryOwner: BinaryClass | BinarySyntheticCompanionClass, target: BinaryMethod)

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
