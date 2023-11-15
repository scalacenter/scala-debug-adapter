package ch.epfl.scala.debugadapter.internal.stacktrace

import tastyquery.Symbols.*
import tastyquery.Types.*
import tastyquery.Contexts.Context
import tastyquery.Trees.Tree
import tastyquery.SourcePosition

sealed trait BinarySymbol:
  def pos: SourcePosition

sealed trait BinaryClassSymbol extends BinarySymbol:
  def symbol: Symbol
  def pos: SourcePosition = symbol.tree.map(_.pos).getOrElse(SourcePosition.NoPosition)
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

  def classSymbol: Option[ClassSymbol] = this match
    case BinaryClass(symbol) => Some(symbol)
    case BinaryInlinedClass(underlying, _) => underlying.classSymbol
    case _ => None

  def companionClass(using Context): Option[BinaryClass] = this match
    case BinaryClass(symbol) => symbol.companionClass.map(BinaryClass.apply)
    case BinarySyntheticCompanionClass(symbol) => Some(BinaryClass(symbol))
    case _ => None

final case class BinaryClass(symbol: ClassSymbol) extends BinaryClassSymbol
final case class BinarySyntheticCompanionClass(symbol: ClassSymbol) extends BinaryClassSymbol
final case class BinarySAMClass(symbol: TermSymbol, parentClass: ClassSymbol, declaredType: Type)
    extends BinaryClassSymbol
final case class BinaryPartialFunction(symbol: TermSymbol, declaredType: Type) extends BinaryClassSymbol
final case class BinaryInlinedClass(underlying: BinaryClassSymbol, inliningTree: Tree) extends BinaryClassSymbol:
  override def symbol: Symbol = underlying.symbol
  override def pos: SourcePosition = underlying.pos
  def inliningPos: SourcePosition = inliningTree.pos

sealed trait BinaryMethodSymbol extends BinarySymbol:
  def symbolOpt: Option[TermSymbol] = None
  def treeOpt: Option[Tree] = symbolOpt.flatMap(_.tree)
  def declaredType: TypeOrMethodic
  def pos: SourcePosition = treeOpt.map(_.pos).getOrElse(SourcePosition.NoPosition)

final case class BinaryMethod(binaryOwner: BinaryClassSymbol, symbol: TermSymbol) extends BinaryMethodSymbol:
  override def declaredType: TypeOrMethodic = symbol.declaredType
  override def symbolOpt: Option[TermSymbol] = Some(symbol)

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
  override def declaredType: TypeOrMethodic = target.declaredType
  override def symbolOpt: Option[TermSymbol] = target.symbolOpt

final case class BinaryOuter(binaryOwner: BinaryClassSymbol, declaredType: Type) extends BinaryMethodSymbol

final case class BinarySuperArg(binaryOwner: BinaryClassSymbol, init: TermSymbol, tree: Tree, declaredType: Type)
    extends BinaryMethodSymbol:
  override def treeOpt: Option[Tree] = Some(tree)

final case class BinaryLiftedTry(binaryOwner: BinaryClassSymbol, tree: Tree, declaredType: Type)
    extends BinaryMethodSymbol:
  override def treeOpt: Option[Tree] = Some(tree)

final case class BinaryByNameArg(binaryOwner: BinaryClassSymbol, tree: Tree, declaredType: Type)
    extends BinaryMethodSymbol:
  override def treeOpt: Option[Tree] = Some(tree)

final case class BinaryMethodBridge(target: BinaryMethodSymbol, declaredType: TypeOrMethodic)
    extends BinaryMethodSymbol:
  override def symbolOpt: Option[TermSymbol] = target.symbolOpt

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

final case class BinaryDeserializeLambda(binaryOnwer: BinaryClassSymbol, declaredType: TypeOrMethodic)
    extends BinaryMethodSymbol

final case class BinarySetter(binaryOwner: BinaryClassSymbol, symbol: TermSymbol, declaredType: TypeOrMethodic)
    extends BinaryMethodSymbol:
  override def symbolOpt: Option[TermSymbol] = Option.when(symbol.isMethod)(symbol)

final case class BinaryGetter(binaryOwner: BinaryClassSymbol, symbol: TermSymbol, declaredType: TypeOrMethodic)
    extends BinaryMethodSymbol:
  override def symbolOpt: Option[TermSymbol] = Some(symbol)

final case class BinarySuperAccessor(binaryOwner: BinaryClassSymbol, symbol: TermSymbol, declaredType: TypeOrMethodic)
    extends BinaryMethodSymbol:
  override def symbolOpt: Option[TermSymbol] = Some(symbol)

final case class BinarySpecializedMethod(binaryOwner: BinaryClassSymbol, symbol: TermSymbol) extends BinaryMethodSymbol:
  override def declaredType: TypeOrMethodic = symbol.declaredType
  override def symbolOpt: Option[TermSymbol] = Some(symbol)

final case class BinaryInlineAccessor(owner: BinaryClass, target: BinaryMethodSymbol) extends BinaryMethodSymbol:
  override def declaredType: TypeOrMethodic = target.declaredType
  override def symbolOpt: Option[TermSymbol] = target.symbolOpt

final case class BinaryAdaptedFun(target: BinaryMethodSymbol) extends BinaryMethodSymbol:
  override def declaredType: TypeOrMethodic = target.declaredType
  override def symbolOpt: Option[TermSymbol] = target.symbolOpt

final case class BinarySAMClassConstructor(binaryOwner: BinaryClassSymbol, declaredType: Type)
    extends BinaryMethodSymbol:
  override def symbolOpt: Option[TermSymbol] = None

final case class BinaryInlinedMethod(underlying: BinaryMethodSymbol, inliningTree: Tree) extends BinaryMethodSymbol:
  override def declaredType: TypeOrMethodic = underlying.declaredType
  override def symbolOpt: Option[TermSymbol] = underlying.symbolOpt
  override def pos: SourcePosition = underlying.pos
  def inliningPos: SourcePosition = inliningTree.pos
