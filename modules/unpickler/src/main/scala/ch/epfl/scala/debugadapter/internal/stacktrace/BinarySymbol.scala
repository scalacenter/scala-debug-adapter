package ch.epfl.scala.debugadapter.internal.stacktrace

import tastyquery.Symbols.*
import tastyquery.Types.*
import tastyquery.Contexts.Context
import tastyquery.Trees.Tree
import tastyquery.SourcePosition

sealed trait BinarySymbol:
  def pos: SourcePosition

sealed trait BinaryClassSymbol extends BinarySymbol:
  private[stacktrace] def symbol: TermSymbol | ClassSymbol
  final def pos: SourcePosition =
    // TODO move to TASTy Query
    symbol.tree.map(_.pos).getOrElse(SourcePosition.NoPosition)

  override def toString: String =
    def format(sym: Symbol) =
      val span = sym.tree.map(tree => s"(${tree.pos.startLine}, ${tree.pos.endLine})").getOrElse("")
      s"$sym $span"
    this match
      case BinaryClass(sym) => s"BinaryClass(${format(sym)})"
      case BinarySyntheticCompanionClass(sym) => s"BinarySyntheticCompanionClass(${format(sym)})"
      case BinarySAMClass(sym, _, tpe) => s"BinarySAMClass(${format(sym)}, ${tpe.showBasic})"
      case BinaryPartialFunction(sym, tpe) => s"BinaryPartialFunction(${format(sym)}, ${tpe.showBasic})"
      case BinaryInlinedClass(underlying, _) =>
        if underlying.isInstanceOf[BinaryInlinedClass] then underlying.toString
        else s"$underlying (inlined)"

// TODO equals/hashcode manually, private constuctors
/**
 * object DecodedClass:
 *   class ClassDef
 *   class SyntheticCompanion
 *   class SAMFunction
 *   class PartialFunction
 *   class Inlined
 */
final case class BinaryClass(symbol: ClassSymbol) extends BinaryClassSymbol
final case class BinarySyntheticCompanionClass(companionSymbol: ClassSymbol) extends BinaryClassSymbol:
  private[stacktrace] override def symbol: ClassSymbol = companionSymbol
final case class BinarySAMClass(symbol: TermSymbol, parentClass: ClassSymbol, tpe: Type) extends BinaryClassSymbol
final case class BinaryPartialFunction(symbol: TermSymbol, tpe: Type) extends BinaryClassSymbol
final case class BinaryInlinedClass(underlying: BinaryClassSymbol, callTree: Tree) extends BinaryClassSymbol:
  private[stacktrace] override def symbol: TermSymbol | ClassSymbol = underlying.symbol
  def callPos: SourcePosition = callTree.pos

sealed trait BinaryMethodSymbol extends BinarySymbol:
  // TODO owner
  def binaryOwner: BinaryClassSymbol
  def symbolOpt: Option[TermSymbol] = None
  def treeOpt: Option[Tree] = symbolOpt.flatMap(_.tree)
  def declaredType: TypeOrMethodic
  final def pos: SourcePosition = treeOpt.map(_.pos).getOrElse(SourcePosition.NoPosition)

// ValOrDefDef
final case class BinaryMethod(binaryOwner: BinaryClassSymbol, symbol: TermSymbol) extends BinaryMethodSymbol:
  override def declaredType: TypeOrMethodic = symbol.declaredType
  override def symbolOpt: Option[TermSymbol] = Some(symbol)

// TODO remove
final case class BinaryLocalLazyInit(binaryOwner: BinaryClassSymbol, symbol: TermSymbol) extends BinaryMethodSymbol:
  override def declaredType: TypeOrMethodic = symbol.declaredType
  override def symbolOpt: Option[TermSymbol] = Some(symbol)

final case class BinaryLazyInit(binaryOwner: BinaryClassSymbol, symbol: TermSymbol) extends BinaryMethodSymbol:
  override def declaredType: TypeOrMethodic = symbol.declaredType
  override def symbolOpt: Option[TermSymbol] = Some(symbol)

final case class BinaryTraitParamAccessor(binaryOwner: BinaryClassSymbol, symbol: TermSymbol)
    extends BinaryMethodSymbol:
  override def declaredType: TypeOrMethodic = symbol.declaredType
  override def symbolOpt: Option[TermSymbol] = Some(symbol)

final case class BinaryMixinForwarder(binaryOwner: BinaryClassSymbol, target: BinaryMethodSymbol)
    extends BinaryMethodSymbol:
  override def declaredType: TypeOrMethodic = target.declaredType
  override def symbolOpt: Option[TermSymbol] = target.symbolOpt

final case class BinaryTraitStaticForwarder(target: BinaryMethodSymbol) extends BinaryMethodSymbol:
  override def binaryOwner: BinaryClassSymbol = target.binaryOwner
  override def declaredType: TypeOrMethodic = target.declaredType
  override def symbolOpt: Option[TermSymbol] = target.symbolOpt

// OuterAccessor
final case class BinaryOuter(binaryOwner: BinaryClassSymbol, declaredType: Type) extends BinaryMethodSymbol

// SuperConstructorArg
// TODO maybe remove constructor
final case class BinarySuperArg(
    binaryOwner: BinaryClassSymbol,
    constructor: TermSymbol,
    argTree: Tree,
    declaredType: Type
) extends BinaryMethodSymbol:
  override def treeOpt: Option[Tree] = Some(argTree)

final case class BinaryLiftedTry(binaryOwner: BinaryClassSymbol, tryTree: Tree, declaredType: Type)
    extends BinaryMethodSymbol:
  override def treeOpt: Option[Tree] = Some(tryTree)

final case class BinaryByNameArg(binaryOwner: BinaryClassSymbol, argTree: Tree, declaredType: Type)
    extends BinaryMethodSymbol:
  override def treeOpt: Option[Tree] = Some(argTree)

// Bridge
final case class BinaryMethodBridge(target: BinaryMethodSymbol, declaredType: TypeOrMethodic)
    extends BinaryMethodSymbol:
  override def binaryOwner: BinaryClassSymbol = target.binaryOwner
  override def symbolOpt: Option[TermSymbol] = target.symbolOpt

// TODO split: PartialFunctionImpl SAMFunctionImpl
final case class BinaryAnonOverride(
    binaryOwner: BinaryClassSymbol,
    overriddenSymbol: TermSymbol,
    declaredType: TypeOrMethodic
) extends BinaryMethodSymbol:
  override def symbolOpt: Option[TermSymbol] = Some(overriddenSymbol)
  override def treeOpt: Option[Tree] = binaryOwner.symbol.tree

final case class BinaryStaticForwarder(
    binaryOwner: BinaryClassSymbol,
    target: BinaryMethodSymbol,
    declaredType: TypeOrMethodic
) extends BinaryMethodSymbol:
  override def symbolOpt: Option[TermSymbol] = target.symbolOpt

final case class BinaryDeserializeLambda(binaryOwner: BinaryClassSymbol, declaredType: TypeOrMethodic)
    extends BinaryMethodSymbol

// SetterAccessor
final case class BinarySetter(binaryOwner: BinaryClassSymbol, symbol: TermSymbol, declaredType: TypeOrMethodic)
    extends BinaryMethodSymbol:
  override def symbolOpt: Option[TermSymbol] = Option.when(symbol.isMethod)(symbol)

// GetterAccessor
final case class BinaryGetter(binaryOwner: BinaryClassSymbol, symbol: TermSymbol, declaredType: TypeOrMethodic)
    extends BinaryMethodSymbol:
  override def symbolOpt: Option[TermSymbol] = Some(symbol)

final case class BinarySuperAccessor(binaryOwner: BinaryClassSymbol, symbol: TermSymbol, declaredType: TypeOrMethodic)
    extends BinaryMethodSymbol:
  override def symbolOpt: Option[TermSymbol] = Some(symbol)

final case class BinarySpecializedMethod(binaryOwner: BinaryClassSymbol, symbol: TermSymbol) extends BinaryMethodSymbol:
  override def declaredType: TypeOrMethodic = symbol.declaredType
  override def symbolOpt: Option[TermSymbol] = Some(symbol)

final case class BinaryInlineAccessor(binaryOwner: BinaryClassSymbol, target: BinaryMethodSymbol)
    extends BinaryMethodSymbol:
  override def declaredType: TypeOrMethodic = target.declaredType
  override def symbolOpt: Option[TermSymbol] = target.symbolOpt

final case class BinaryAdaptedFun(target: BinaryMethodSymbol) extends BinaryMethodSymbol:
  override def binaryOwner: BinaryClassSymbol = target.binaryOwner
  override def declaredType: TypeOrMethodic = target.declaredType
  override def symbolOpt: Option[TermSymbol] = target.symbolOpt

final case class BinarySAMClassConstructor(binaryOwner: BinaryClassSymbol, declaredType: Type)
    extends BinaryMethodSymbol:
  override def symbolOpt: Option[TermSymbol] = None

final case class BinaryInlinedMethod(underlying: BinaryMethodSymbol, callTree: Tree) extends BinaryMethodSymbol:
  override def binaryOwner: BinaryClassSymbol = underlying.binaryOwner
  override def declaredType: TypeOrMethodic = underlying.declaredType
  override def symbolOpt: Option[TermSymbol] = underlying.symbolOpt
  def callPos: SourcePosition = callTree.pos
