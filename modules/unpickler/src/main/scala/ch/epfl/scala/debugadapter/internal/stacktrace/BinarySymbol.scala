package ch.epfl.scala.debugadapter.internal.stacktrace

import tastyquery.Symbols.*

enum BinaryClassSymbol:
  case BinaryClass(symbol: ClassSymbol, kind: BinaryClassKind)
  case BinarySAMClass(symbol: TermSymbol, samClass: ClassSymbol)

enum BinaryClassKind:
  case TopLevelOrInner, Local, Anon

enum BinaryMethodSymbol:
  case BinaryMethod(binaryOwner: BinaryClassSymbol, symbol: TermSymbol, kind: BinaryMethodKind)
  case BinaryByNameArg(binaryOwner: BinaryClassSymbol)

enum BinaryMethodKind:
  case InstanceDef, LocalDef, AnonFun, Getter, Setter, LazyInit, LocalLazyInit, Constructor, TraitConstructor,
    MixinForwarder, TraitStaticAccessor, SuperAccessor, DefaultParameter
