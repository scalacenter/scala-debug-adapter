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

sealed trait BinaryMethodSymbol extends BinarySymbol

final case class BinaryMethod(binaryOwner: BinaryClassSymbol, symbol: TermSymbol) extends BinaryMethodSymbol
final case class BinaryAnonFun(binaryOwner: BinaryClassSymbol, symbol: TermSymbol, adapted: Boolean)
    extends BinaryMethodSymbol
final case class BinaryLocalLazyInit(binaryOwner: BinaryClassSymbol, symbol: TermSymbol) extends BinaryMethodSymbol
final case class BinaryLazyInit(binaryOwner: BinaryClassSymbol, symbol: TermSymbol) extends BinaryMethodSymbol
final case class BinaryTraitParamAccessor(binaryOwner: BinaryClassSymbol, symbol: TermSymbol) extends BinaryMethodSymbol
final case class BinaryMixinForwarder(binaryOwner: BinaryClassSymbol, symbol: TermSymbol) extends BinaryMethodSymbol
final case class BinaryTraitStaticForwarder(binaryOwner: BinaryClassSymbol, symbol: TermSymbol)
    extends BinaryMethodSymbol
final case class BinaryOuter(binaryOwner: BinaryClassSymbol, outerClass: ClassSymbol) extends BinaryMethodSymbol
final case class BinarySuperArg(binaryOwner: BinaryClassSymbol, init: TermSymbol, tpe: Type) extends BinaryMethodSymbol
final case class BinaryLiftedTry(binaryOwner: BinaryClassSymbol, tpe: Type) extends BinaryMethodSymbol
final case class BinaryByNameArg(binaryOwner: BinaryClassSymbol, tpe: Type, adapted: Boolean) extends BinaryMethodSymbol
final case class BinaryMethodBridge(binaryOwner: BinaryClassSymbol, targetSymbol: TermSymbol, tpe: TypeOrMethodic)
    extends BinaryMethodSymbol
final case class BinaryAnonOverride(binaryOwner: BinaryClassSymbol, overriddenSymbol: TermSymbol, tpe: TypeOrMethodic)
    extends BinaryMethodSymbol
final case class BinaryStaticForwarder(binaryOwner: BinaryClassSymbol, targetSymbol: TermSymbol, tpe: TypeOrMethodic)
    extends BinaryMethodSymbol
final case class BinaryDeserializeLambda(binaryOnwer: BinaryClassSymbol) extends BinaryMethodSymbol
final case class BinaryTraitSetter(binaryOwner: BinaryClassSymbol, sym: TermSymbol, paramType: TypeOrMethodic)
    extends BinaryMethodSymbol
final case class BinarySuperAccessor(
    binaryOwner: BinaryClassSymbol,
    sym: TermSymbol,
    tpe: TypeOrMethodic,
    isBridge: Boolean
) extends BinaryMethodSymbol
final case class BinarySpecializedMethod(binaryOwner: BinaryClassSymbol, sym: TermSymbol) extends BinaryMethodSymbol
