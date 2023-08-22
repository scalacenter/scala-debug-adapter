package ch.epfl.scala.debugadapter.internal.stacktrace

import tastyquery.Symbols.*
import tastyquery.Types.Type

enum BinaryClassSymbol:
  case BinaryClass(symbol: ClassSymbol, kind: BinaryClassKind)
  case BinarySAMClass(symbol: TermSymbol, samType: Type)
  def symbol: Symbol
  def symbolKind = this match
    case BinaryClass(_, kind) => kind
    case _ => BinaryClassKind.SAMClass

enum BinaryClassKind:
  case TopLevelOrInner, Local, Anon, SAMClass

enum BinaryMethodSymbol:
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
