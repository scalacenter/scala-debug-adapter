package ch.epfl.scala.debugadapter.internal.stacktrace

import tastyquery.Symbols.*

enum BinaryClassSymbol:
  case BinaryClass(symbol: ClassSymbol, kind: BinaryClassKind)
  case BinarySAMClass(symbol: TermSymbol)
  
enum BinaryClassKind:
  case TopLevel, Inner, Local, Anon

enum BinaryMethodSymbol:  
  case BinaryMethod(binaryOwner: BinaryClassSymbol, symbol: TermSymbol, kind: BinaryMethodKind)
  case BinaryByNameArg(binaryOwner: BinaryClassSymbol)

enum BinaryMethodKind:
  case InstanceDef, LocalDef, Anon, Getter, Setter, LazyInit, LocalLazyInit, Constructor, TraitConstructor,
       MixinForwarder, TraitStaticAccessor, SuperAccessor, DefaultParameter
