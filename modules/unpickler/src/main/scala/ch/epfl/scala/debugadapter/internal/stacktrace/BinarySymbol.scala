package ch.epfl.scala.debugadapter.internal.stacktrace

import tastyquery.Symbols.*
import tastyquery.Types.Type
import tastyquery.Types.TypeOrMethodic

sealed trait BinarySymbol

enum BinaryClassSymbol(val symbol: Symbol, val kind: BinaryClassKind) extends BinarySymbol:
  case BinaryClass(override val symbol: ClassSymbol, override val kind: BinaryClassKind)
      extends BinaryClassSymbol(symbol, kind)
  case BinarySAMClass(term: TermSymbol, samClassSymbol: ClassSymbol, samType: Type)
      extends BinaryClassSymbol(term, BinaryClassKind.SAMClass)

  override def toString: String =
    val span = symbol.tree.map(tree => s"(${tree.pos.startLine}, ${tree.pos.endLine})").getOrElse("")
    this match
      case BinarySAMClass(symbol, samClassSymbol, samType) =>
        s"BinarySAMClass($symbol $span, $samClassSymbol, ${samType.showBasic})"
      case BinaryClass(symbol, kind) => s"BinaryClass($symbol $span, $kind)"

enum BinaryClassKind:
  case TopLevelOrInner, Local, Anon, SAMClass, SyntheticCompanionClass

enum BinaryMethodSymbol extends BinarySymbol:
  case BinaryMethod(binaryOwner: BinaryClassSymbol, term: TermSymbol, kind: BinaryMethodKind)
  case BinaryOuter(binaryOwner: BinaryClassSymbol, outerClass: ClassSymbol)
  case BinarySuperArg(binaryOwner: BinaryClassSymbol, init: TermSymbol, tpe: Type)
  case BinaryLiftedTry(binaryOwner: BinaryClassSymbol, tpe: Type)
  case BinaryByNameArg(binaryOwner: BinaryClassSymbol, tpe: Type, isAdapted: Boolean)
  case BinaryMethodBridge(binaryOwner: BinaryClassSymbol, term: TermSymbol, tpe: TypeOrMethodic)

  def symbol = this match
    case BinaryMethod(_, term, _) => Some(term)
    case _ => None

  def symbolKind = this match
    case BinaryMethod(_, _, kind) => kind
    case BinaryOuter(_, _) => BinaryMethodKind.Outer
    case BinarySuperArg(_, _, _) => BinaryMethodKind.SuperArg
    case BinaryLiftedTry(_, _) => BinaryMethodKind.LiftedTry
    case BinaryByNameArg(_, _, adapted) =>
      if adapted then BinaryMethodKind.AdaptedByNameArg else BinaryMethodKind.ByNameArg
    case BinaryMethodBridge(_, _, _) => BinaryMethodKind.Bridge

enum BinaryMethodKind:
  case InstanceDef, LocalDef, AnonFun, AdaptedAnonFun, Getter, Setter, LazyInit, LocalLazyInit, Constructor,
    TraitConstructor, MixinForwarder, TraitStaticAccessor, SuperAccessor, DefaultParameter, Outer, SuperArg,
    TraitParamGetter, TraitParamSetter, LiftedTry, ByNameArg, AdaptedByNameArg, Bridge
