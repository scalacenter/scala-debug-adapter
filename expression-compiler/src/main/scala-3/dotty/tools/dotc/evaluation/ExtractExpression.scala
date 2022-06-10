package dotty.tools.dotc.evaluation

import dotty.tools.dotc.EvaluationContext
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.transform.SymUtils.*
import dotty.tools.dotc.core.DenotTransformers.DenotTransformer
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.transform.MacroTransform
import dotty.tools.dotc.core.Phases.*
import dotty.tools.dotc.report
import dotty.tools.dotc.util.SrcPos

class ExtractExpression(using evalCtx: EvaluationContext)
    extends MacroTransform
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
        // that are extracted out of the expression val to the evaluate method
        ref.copySymDenotation(owner = evalCtx.evaluateMethod)
      case _ =>
        ref

  override def transformPhase(using Context): Phase = this.next

  override protected def newTransformer(using Context): Transformer =
    new Transformer:
      var expressionTree: Tree = _
      override def transform(tree: Tree)(using Context): Tree =
        tree match
          case PackageDef(pid, stats) =>
            val evaluationClassDef =
              stats.find(_.symbol == evalCtx.evaluationClass)
            val others = stats.filter(_.symbol != evalCtx.evaluationClass)
            val transformedStats = (others ++ evaluationClassDef).map(transform)
            PackageDef(pid, transformedStats)
          case tree: ValDef if isExpressionVal(tree.symbol) =>
            expressionTree = tree.rhs
            evalCtx.store(tree.symbol)
            unitLiteral
          case tree: DefDef if tree.symbol == evalCtx.evaluateMethod =>
            val transformedExpr =
              ExpressionTransformer.transform(expressionTree)
            cpy.DefDef(tree)(rhs = transformedExpr)
          case tree =>
            super.transform(tree)

  private object ExpressionTransformer extends TreeMap:
    override def transform(tree: Tree)(using Context): Tree =
      val desugaredIdent = tree match
        case tree: Ident => desugarIdent(tree)
        case _ => tree

      desugaredIdent match
        // static object
        case tree: (Ident | Select) if isStaticObject(tree.symbol) =>
          getStaticObject(tree)(tree.symbol.moduleClass)

        // non-static object
        case tree: (Ident | Select) if isNonStaticObject(tree.symbol) =>
          val qualifier = getTransformedQualifier(tree)
          callMethod(tree)(qualifier, tree.symbol.asTerm, List.empty, tree.tpe)

        // local variable
        case tree: Ident if isLocalVariable(tree.symbol) =>
          if tree.symbol.is(Lazy) then
            report.error(
              s"Evaluation of local lazy val not supported: ${tree.symbol}",
              tree.srcPos
            )
            tree
          else
            // a local variable can be captured by a class or method
            val owner = tree.symbol.owner
            val candidates = evalCtx.expressionSymbol.ownersIterator
              .takeWhile(_ != owner)
              .filter(s => s.isClass || s.is(Method))
              .toSeq
            val capturer = candidates
              .findLast(_.isClass)
              .orElse(candidates.find(_.is(Method)))
            capturer match
              case Some(capturer) =>
                if capturer.isClass then
                  getClassCapture(tree)(tree.symbol, capturer.asClass, tree.tpe)
                else
                  getMethodCapture(tree)(tree.symbol, capturer.asTerm, tree.tpe)
              case None => getLocalValue(tree)(tree.symbol, tree.tpe)

        // assignement to local variable
        case tree @ Assign(lhs, rhs) if isLocalVariable(lhs.symbol) =>
          report.error(
            s"Assignment to local variable not supported: ${lhs.symbol}",
            tree.srcPos
          )
          Assign(lhs, transform(rhs))

        // inaccessible fields
        case tree: Select if isInaccessibleField(tree.symbol) =>
          val qualifier = getTransformedQualifier(tree)
          getField(tree)(qualifier, tree.symbol.asTerm, tree.tpe)

        // assignment to inaccessible fields
        case tree @ Assign(lhs, rhs) if isInaccessibleField(lhs.symbol) =>
          val qualifier = getTransformedQualifier(lhs)
          setField(tree)(qualifier, lhs.symbol.asTerm, transform(rhs), tree.tpe)

        // this or outer this
        case tree @ This(Ident(name)) =>
          thisOrOuterValue(tree)(tree.symbol.enclosingClass.asClass)

        // inaccessible constructors
        case tree: (Select | Apply | TypeApply)
            if isInaccessibleConstructor(tree.symbol) =>
          val args = getTransformedArgs(tree)
          val qualifier = getTransformedQualifierOfNew(tree)
          callConstructor(tree)(qualifier, tree.symbol.asTerm, args, tree.tpe)

        // inaccessible methods
        case tree: (Ident | Select | Apply | TypeApply)
            if isInaccessibleMethod(tree.symbol) =>
          val args = getTransformedArgs(tree)
          val qualifier = getTransformedQualifier(tree)
          callMethod(tree)(qualifier, tree.symbol.asTerm, args, tree.tpe)

        case Typed(tree, tpt)
            if tpt.symbol.isType && !isTypeAccessible(tpt.symbol.asType) =>
          transform(tree)
        case tree =>
          super.transform(tree)

    private def getTransformedArgs(tree: Tree)(using Context): List[Tree] =
      tree match
        case _: (Ident | Select) => List.empty
        case Apply(fun, args) => getTransformedArgs(fun) ++ args.map(transform)
        case TypeApply(fun, _) => getTransformedArgs(fun)

    private def getTransformedQualifier(tree: Tree)(using Context): Tree =
      tree match
        case Ident(_) =>
          val classOwner = tree.symbol.enclosingClass.asClass
          if isStaticObject(classOwner)
          then getStaticObject(tree)(classOwner)
          else thisOrOuterValue(tree)(classOwner)
        case Select(qualifier, _) =>
          val classOwner = tree.symbol.enclosingClass.asClass
          if isStaticObject(classOwner)
          then getStaticObject(tree)(classOwner)
          else transform(qualifier)
        case Apply(fun, _) => getTransformedQualifier(fun)
        case TypeApply(fun, _) => getTransformedQualifier(fun)

    private def getTransformedQualifierOfNew(tree: Tree)(using Context): Tree =
      tree match
        case Select(New(tpt), _) => getTransformedPrefix(tpt)
        case Apply(fun, _) => getTransformedQualifierOfNew(fun)
        case TypeApply(fun, _) => getTransformedQualifierOfNew(fun)

    private def getTransformedPrefix(typeTree: Tree)(using Context): Tree =
      typeTree match
        case Ident(_) =>
          thisOrOuterValue(typeTree)(
            typeTree.symbol.owner.enclosingClass.asClass
          )
        case Select(qualifier, _) => transform(qualifier)
        case AppliedTypeTree(tpt, _) => getTransformedPrefix(tpt)

  end ExpressionTransformer

  private def isExpressionVal(sym: Symbol)(using Context): Boolean =
    sym.name == evalCtx.expressionTermName

  // symbol can be a class or a method
  private def thisOrOuterValue(tree: Tree)(cls: ClassSymbol)(using
      Context
  ): Tree =
    reportErrorIfLocalInsideValueClass(
      evalCtx.expressionSymbol.owner,
      tree.srcPos
    )
    val ths = getThis(tree)(evalCtx.classOwners.head)
    val target = evalCtx.classOwners.indexOf(cls)
    if target >= 0 then
      evalCtx.classOwners
        .drop(1)
        .take(target)
        .foldLeft(ths) { (innerObj, outerSym) =>
          getOuter(tree)(innerObj, outerSym, outerSym.thisType)
        }
    else nullLiteral

  private def getThis(tree: Tree)(cls: ClassSymbol)(using Context): Tree =
    reflectEval(tree)(
      None,
      EvaluationStrategy.This(cls),
      List.empty,
      Some(evalCtx.classOwners.head.thisType)
    )

  private def getOuter(
      tree: Tree
  )(qualifier: Tree, outerCls: ClassSymbol, tpe: Type)(using
      Context
  ): Tree =
    reflectEval(tree)(
      Some(qualifier),
      EvaluationStrategy.Outer(outerCls),
      List.empty,
      Some(tpe)
    )

  private def getLocalValue(
      tree: Tree
  )(variable: Symbol, tpe: Type)(using Context): Tree =
    reflectEval(tree)(
      None,
      EvaluationStrategy.LocalValue(variable.asTerm),
      List.empty,
      Some(tpe)
    )

  private def getClassCapture(
      tree: Tree
  )(variable: Symbol, cls: ClassSymbol, tpe: Type)(using
      Context
  ): Tree =
    reportErrorIfLocalInsideValueClass(cls, tree.srcPos)
    reflectEval(tree)(
      Some(thisOrOuterValue(tree)(cls)),
      EvaluationStrategy.ClassCapture(variable.asTerm, cls.asClass),
      List.empty,
      Some(tpe)
    )

  private def getMethodCapture(
      tree: Tree
  )(variable: Symbol, method: TermSymbol, tpe: Type)(using
      Context
  ): Tree =
    reportErrorIfLocalInsideValueClass(method, tree.srcPos)
    reflectEval(tree)(
      None,
      EvaluationStrategy.MethodCapture(variable.asTerm, method.asTerm),
      List.empty,
      Some(tpe)
    )

  private def getStaticObject(
      tree: Tree
  )(obj: Symbol)(using ctx: Context): Tree =
    reflectEval(tree)(
      None,
      EvaluationStrategy.StaticObject(obj.asClass),
      List.empty,
      None
    )

  private def getField(
      tree: Tree
  )(qualifier: Tree, field: TermSymbol, tpe: Type)(using
      Context
  ): Tree =
    reportErrorIfLocalInsideValueClass(field, tree.srcPos)
    reflectEval(tree)(
      Some(qualifier),
      EvaluationStrategy.Field(field),
      List.empty,
      Some(tpe)
    )

  private def setField(tree: Tree)(
      qualifier: Tree,
      field: TermSymbol,
      rhs: Tree,
      tpe: Type
  )(using
      Context
  ): Tree =
    reportErrorIfLocalInsideValueClass(field, tree.srcPos)
    reflectEval(tree)(
      Some(qualifier),
      EvaluationStrategy.FieldAssign(field),
      List(rhs),
      Some(tpe)
    )

  private def callMethod(tree: Tree)(
      qualifier: Tree,
      method: TermSymbol,
      args: List[Tree],
      tpe: Type
  )(using Context): Tree =
    reportErrorIfLocalInsideValueClass(method, tree.srcPos)
    reflectEval(tree)(
      Some(qualifier),
      EvaluationStrategy.MethodCall(method),
      args,
      Some(tpe)
    )

  private def callConstructor(tree: Tree)(
      qualifier: Tree,
      ctr: TermSymbol,
      args: List[Tree],
      tpe: Type
  )(using Context): Tree =
    reportErrorIfLocalInsideValueClass(ctr, tree.srcPos)
    reflectEval(tree)(
      Some(qualifier),
      EvaluationStrategy.ConstructorCall(ctr, ctr.owner.asClass),
      args,
      Some(tpe)
    )

  private def reflectEval(tree: Tree)(
      qualifier: Option[Tree],
      strategy: EvaluationStrategy,
      args: List[Tree],
      tpe: Option[Type]
  )(using
      Context
  ): Tree =
    val reflectEval =
      cpy.Apply(tree)(
        Select(This(evalCtx.evaluationClass), termName("reflectEval")),
        List(
          qualifier.getOrElse(nullLiteral),
          Literal(Constant(strategy.toString)),
          JavaSeqLiteral(args, TypeTree(ctx.definitions.ObjectType))
        )
      )
    reflectEval.putAttachment(EvaluationStrategy, strategy)
    tpe.map(cast(reflectEval, _)).getOrElse(reflectEval)

  private def cast(tree: Tree, tpe: Type)(using Context): Tree =
    val widenDealiasTpe = tpe.widenDealias
    if isTypeAccessible(widenDealiasTpe.typeSymbol.asType)
    then tree.cast(widenDealiasTpe)
    else tree

  /**
   * In the [[ResolveReflectEval]] phase we cannot find the symbol of a local method
   * or local class inside a value class. So we report an error early.
   */
  private def reportErrorIfLocalInsideValueClass(
      symbol: Symbol,
      srcPos: SrcPos
  )(using Context): Unit =
    for
      localClassOrMethod <- symbol.ownersIterator
        .find { sym =>
          (sym.isClass || sym.is(Method)) && sym.enclosure.is(Method)
        }
      valueClass <- localClassOrMethod.ownersIterator.find(_.isValueClass)
    do
      report.error(
        s"""|Evaluation involving a local method or local class in a value class not supported:
            |$symbol belongs to $localClassOrMethod which is local inside value $valueClass""".stripMargin,
        srcPos
      )

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

  private def isLocalVariable(symbol: Symbol)(using Context): Boolean =
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
