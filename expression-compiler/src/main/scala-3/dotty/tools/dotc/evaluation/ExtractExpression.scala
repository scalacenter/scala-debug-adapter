package dotty.tools.dotc.evaluation

import dotty.tools.dotc.EvaluationContext
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.transform.SymUtils.*
import dotty.tools.dotc.core.DenotTransformers.DenotTransformer
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.SymDenotations.SymDenotation

class ExtractExpression(using evalCtx: EvaluationContext)
    extends MiniPhase
    with DenotTransformer:
  override def phaseName: String = ExtractExpression.name

  /**
   * Change the return type of the `evaluate` method
   * and update the owner and types of the symDenotations inserted into `evaluate`.
   */
  override def transform(ref: SingleDenotation)(using
      Context
  ): SingleDenotation =
    ref match
      case ref: SymDenotation if isExpressionVal(ref.symbol.maybeOwner) =>
        // update owner of the symDenotation, e.g. local vals
        // after it was inserted to `evaluate` method
        ref.copySymDenotation(owner = evalCtx.evaluateMethod)
      case _ =>
        ref

  override def transformValDef(tree: ValDef)(using Context): Tree =
    if tree.symbol == evalCtx.expressionSymbol
    then unitLiteral
    else super.transformValDef(tree)

  override def transformDefDef(tree: DefDef)(using Context): Tree =
    if tree.symbol == evalCtx.evaluateMethod
    then
      val expressionTree =
        evalCtx.expressionSymbol.defTree.asInstanceOf[ValDef].rhs
      val transformedExpr = ExpressionTransformer.transform(expressionTree)
      DefDef(
        tree.symbol.asInstanceOf[TermSymbol],
        List(),
        tree.tpt.tpe,
        transformedExpr
      )
    else super.transformDefDef(tree)

  object ExpressionTransformer extends TreeMap:
    override def transform(tree: Tree)(using Context): Tree =
      tree match
        // static object
        case tree: Ident if isStaticObject(tree.symbol) =>
          getStaticObject(tree.symbol.moduleClass)
        case tree: Select if isStaticObject(tree.symbol) =>
          getStaticObject(tree.symbol.moduleClass)

        // non-static object
        case tree: Ident if isNonStaticObject(tree.symbol) =>
          callMethod(getThis, tree.symbol.asTerm, List.empty, tree.tpe)
        case tree: Select if isNonStaticObject(tree.symbol) =>
          val qualifier = transform(tree.qualifier)
          callMethod(qualifier, tree.symbol.asTerm, List.empty, tree.tpe)

        // local value
        case tree @ Ident(name) if isLocalVal(tree.symbol) =>
          // a local value can be captured by a class or method
          val owner = tree.symbol.owner
          val candidates = evalCtx.expressionSymbol.ownersIterator
            .takeWhile(_ != owner)
            .filter(s => s.isClass || s.is(Method))
            .toSeq
          val capturer = candidates
            .findLast(_.isClass)
            .orElse(candidates.find(_.is(Method)))
          capturer match
            case Some(cls) if cls.isClass =>
              getClassCapture(tree.symbol, cls, tree.tpe)
            case Some(method) => getMethodCapture(tree.symbol, method, tree.tpe)
            case None => getLocalValue(tree.symbol, tree.tpe)

        // inaccessible fields
        case tree @ Ident(name) if isInaccessibleField(tree.symbol) =>
          val qualifier =
            if isStaticObject(tree.symbol.owner)
            then getStaticObject(tree.symbol.owner)
            else getThis
          getField(qualifier, tree.symbol.asTerm, tree.tpe)
        case tree @ Select(qualifier, name)
            if isInaccessibleField(tree.symbol) =>
          val qualifier = transform(tree.qualifier)
          getField(qualifier, tree.symbol.asTerm, tree.tpe)

        // this or outer this
        case tree @ This(Ident(name)) =>
          thisOrOuterValue(tree.symbol)

        // inaccessible constructors
        case tree @ Apply(fun @ Select(New(classTree), _), _)
            if isInaccessibleConstructor(fun.symbol) =>
          val args = tree.args.map(transform)
          // the qualifier can be captured by the constructor
          val qualifier = classTree match
            case Select(qualifier, _) => transform(qualifier)
            case tree @ Ident(_) => thisOrOuterValue(tree.symbol.owner)
            case _ => EmptyTree
          callConstructor(qualifier, fun.symbol.asTerm, args, tree.tpe)

        // inaccessible methods
        case tree @ Apply(fun: Select, _) if isInaccessibleMethod(fun.symbol) =>
          val args = tree.args.map(transform)
          val qualifier = transform(fun.qualifier)
          callMethod(qualifier, fun.symbol.asTerm, args, tree.tpe)
        case tree @ Apply(fun: Ident, _) if isInaccessibleMethod(fun.symbol) =>
          val owner = fun.symbol.owner
          val qualifier =
            if isStaticObject(owner)
            then getStaticObject(owner)
            else thisOrOuterValue(owner)
          val args = tree.args.map(transform)
          callMethod(qualifier, fun.symbol.asTerm, args, tree.tpe)

        case tree => super.transform(tree)

  private def isExpressionVal(sym: Symbol)(using Context): Boolean =
    sym.name == evalCtx.expressionTermName

  // symbol can be a class or a method
  private def thisOrOuterValue(symbol: Symbol)(using Context): Tree =
    val cls = symbol.ownersIterator.find(_.isClass).get
    val owners = evalCtx.classOwners.toSeq
    val target = owners.indexOf(cls)
    owners
      .take(target + 1)
      .drop(1)
      .foldLeft(getThis) { (innerObj, outerSym) =>
        getOuter(innerObj, outerSym.thisType)
      }

  private def getThis(using Context): Tree =
    reflectEval(
      None,
      EvaluationStrategy.This,
      List.empty,
      Some(evalCtx.classOwners.head.thisType)
    )

  private def getOuter(qualifier: Tree, tpe: Type)(using
      Context
  ): Tree =
    reflectEval(
      Some(qualifier),
      EvaluationStrategy.Outer,
      List.empty,
      Some(tpe)
    )

  private def getLocalValue(value: Symbol, tpe: Type)(using Context): Tree =
    reflectEval(
      None,
      EvaluationStrategy.LocalValue(value.asTerm),
      List.empty,
      Some(tpe)
    )

  private def getClassCapture(value: Symbol, cls: Symbol, tpe: Type)(using
      Context
  ): Tree =
    reflectEval(
      Some(thisOrOuterValue(cls)),
      EvaluationStrategy.ClassCapture(value.asTerm, cls.asClass),
      List.empty,
      Some(tpe)
    )

  private def getMethodCapture(value: Symbol, method: Symbol, tpe: Type)(using
      Context
  ): Tree =
    reflectEval(
      None,
      EvaluationStrategy.MethodCapture(value.asTerm, method.asTerm),
      List.empty,
      Some(tpe)
    )

  private def getStaticObject(obj: Symbol)(using ctx: Context): Tree =
    reflectEval(
      None,
      EvaluationStrategy.StaticObject(obj.asClass),
      List.empty,
      None
    )

  private def getField(qualifier: Tree, field: TermSymbol, tpe: Type)(using
      Context
  ): Tree =
    reflectEval(
      Some(qualifier),
      EvaluationStrategy.Field(field),
      List.empty,
      Some(tpe)
    )

  private def callMethod(
      qualifier: Tree,
      fun: TermSymbol,
      args: List[Tree],
      tpe: Type
  )(using Context): Tree =
    reflectEval(
      Some(qualifier),
      EvaluationStrategy.MethodCall(fun),
      args,
      Some(tpe)
    )

  private def callConstructor(
      qualifier: Tree,
      ctr: TermSymbol,
      args: List[Tree],
      tpe: Type
  )(using Context): Tree =
    reflectEval(
      Some(qualifier),
      EvaluationStrategy.ConstructorCall(ctr, ctr.owner.asClass),
      args,
      Some(tpe)
    )

  private def reflectEval(
      qualifier: Option[Tree],
      strategy: EvaluationStrategy,
      args: List[Tree],
      tpe: Option[Type]
  )(using
      Context
  ): Tree =
    val reflectEval =
      Select(This(evalCtx.evaluationClass), termName("reflectEval"))
    // We put the attachment on the fun of the apply.
    // On the apply itself, it would be lost after the erasure phase.
    reflectEval.putAttachment(EvaluationStrategy, strategy)
    val tree =
      Apply(
        reflectEval,
        List(
          qualifier.getOrElse(nullLiteral),
          Literal(Constant(strategy.toString)),
          JavaSeqLiteral(args, TypeTree(ctx.definitions.ObjectType))
        )
      )
    tpe.map(cast(tree, _)).getOrElse(tree)

  private def cast(tree: Tree, tpe: Type)(using Context): Tree =
    val widenDealiasTpe = tpe.widenDealias
    if isTypeAccessible(widenDealiasTpe.typeSymbol.asType)
    then tree.cast(widenDealiasTpe)
    else tree

  private def isStaticObject(symbol: Symbol)(using Context): Boolean =
    symbol.is(Module) && symbol.isStatic && !symbol.isRoot

  private def isNonStaticObject(symbol: Symbol)(using Context): Boolean =
    symbol.is(Module) && !symbol.isStatic && !symbol.isRoot

  /**
   * The symbol is a field and the expression class cannot access it
   * either because it is private or it belongs to an inacessible type
   */
  private def isInaccessibleField(symbol: Symbol)(using Context): Boolean =
    symbol.isField && symbol.owner.isType && !isTermAccessible(
      symbol.asTerm,
      symbol.owner.asType
    )

  /**
   * The symbol is a real method and the expression class cannot access it
   * either because it is private or it belongs to an inaccessible type
   */
  private def isInaccessibleMethod(symbol: Symbol)(using Context): Boolean =
    symbol.isRealMethod && (
      !symbol.owner.isType ||
        !isTermAccessible(symbol.asTerm, symbol.owner.asType)
    )

  /**
   * The symbol is a constructor and the expression class cannot access it
   * either because it is an inaccessible method or it belong to a nested type (not static)
   */
  private def isInaccessibleConstructor(symbol: Symbol)(using
      Context
  ): Boolean =
    symbol.isConstructor &&
      (isInaccessibleMethod(symbol) || !symbol.owner.isStatic)

  private def isLocalVal(symbol: Symbol)(using Context): Boolean =
    !symbol.is(Method) &&
      symbol.isLocalToBlock &&
      symbol.ownersIterator.forall(_ != evalCtx.evaluateMethod)

  // Check if a term is accessible from the expression class
  private def isTermAccessible(symbol: TermSymbol, owner: TypeSymbol)(using
      Context
  ): Boolean =
    !symbol.isPrivate && isTypeAccessible(owner)

    // Check if a type is accessible from the expression class
  private def isTypeAccessible(symbol: TypeSymbol)(using Context): Boolean =
    !symbol.isLocal && symbol.ownersIterator.forall(sym =>
      sym.isPublic || sym.privateWithin.is(PackageClass)
    )

object ExtractExpression:
  val name: String = "extract-expression"
