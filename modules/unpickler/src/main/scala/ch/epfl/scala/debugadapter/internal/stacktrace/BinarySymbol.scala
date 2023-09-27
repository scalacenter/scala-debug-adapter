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
  case BinaryByNameArg(binaryOwner: BinaryClassSymbol)
  case BinaryOuter(binaryOwner: BinaryClassSymbol, outerClass: ClassSymbol)

  def symbol = this match
    case BinaryMethod(_, term, _) => Some(term)
    case _ => None

  def symbolKind = this match
    case BinaryMethod(_, _, kind) => kind
    case BinaryByNameArg(_) => BinaryMethodKind.ByNameArg
    case BinaryOuter(_, _) => BinaryMethodKind.Outer

enum BinaryMethodKind:
  case InstanceDef, LocalDef, AnonFun, Getter, Setter, LazyInit, LocalLazyInit, Constructor, TraitConstructor,
    MixinForwarder, TraitStaticAccessor, SuperAccessor, DefaultParameter, ByNameArg, Outer
