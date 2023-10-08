package ch.epfl.scala.debugadapter.internal.stacktrace

import tastyquery.Symbols.*
import tastyquery.Types.Type

sealed trait BinarySymbol

enum BinaryClassSymbol extends BinarySymbol:
  case BinaryClass(symbol: ClassSymbol, kind: BinaryClassKind)
  case BinarySAMClass(symbol: TermSymbol, samClassSymbol: ClassSymbol, samType: Type)
  def symbol: Symbol
  def symbolKind = this match
    case BinaryClass(_, kind) => kind
    case _ => BinaryClassKind.SAMClass

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
  case BinaryByNameArg(binaryOwner: BinaryClassSymbol, tpe: Type)

  def symbol = this match
    case BinaryMethod(_, term, _) => Some(term)
    case _ => None

  def symbolKind = this match
    case BinaryMethod(_, _, kind) => kind
    case BinaryOuter(_, _) => BinaryMethodKind.Outer
    case BinarySuperArg(_, _, _) => BinaryMethodKind.SuperArg
    case BinaryLiftedTry(_, _) => BinaryMethodKind.LiftedTry
    case BinaryByNameArg(_, _) => BinaryMethodKind.ByNameArg

enum BinaryMethodKind:
  case InstanceDef, LocalDef, AnonFun, AdaptedAnonFun, Getter, Setter, LazyInit, LocalLazyInit, Constructor,
    TraitConstructor, MixinForwarder, TraitStaticAccessor, SuperAccessor, DefaultParameter, Outer, SuperArg,
    TraitParamGetter, TraitParamSetter, LiftedTry, ByNameArg
