package dotty.tools.dotc.evaluation

import dotty.tools.dotc.ExpressionContext
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.DenotTransformers.DenotTransformer
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.evaluation.SymUtils.*
import dotty.tools.dotc.transform.MacroTransform
import dotty.tools.dotc.core.Phases.*
import dotty.tools.dotc.report
import dotty.tools.dotc.util.SrcPos
import scala.annotation.nowarn

class ExtractExpression(using exprCtx: ExpressionContext) extends MacroTransform with DenotTransformer:
  override def phaseName: String = ExtractExpression.name

  /**
   * update the owner of the symbols inserted into `evaluate`.
   */
  override def transform(ref: SingleDenotation)(using
      Context
  ): SingleDenotation =
    ref match
      case ref: SymDenotation if isExpressionVal(ref.symbol.maybeOwner) =>
        // update owner of the symDenotation, e.g. local vals
        // that are extracted out of the expression val to the evaluate method
        ref.copySymDenotation(owner = exprCtx.evaluateMethod)
      case _ =>
        ref

  override def transformPhase(using Context): Phase = this.next

  override protected def newTransformer(using Context): Transformer =
    new Transformer:
      var expressionTree: Tree = null
      override def transform(tree: Tree)(using Context): Tree =
        tree match
          case PackageDef(pid, stats) =>
            val evaluationClassDef =
              stats.find(_.symbol == exprCtx.expressionClass)
            val others = stats.filter(_.symbol != exprCtx.expressionClass)
            val transformedStats = (others ++ evaluationClassDef).map(transform)
            PackageDef(pid, transformedStats)
          case tree: ValDef if isExpressionVal(tree.symbol) =>
            expressionTree = tree.rhs
            exprCtx.store(tree.symbol)
            unitLiteral
          case tree: DefDef if tree.symbol == exprCtx.evaluateMethod =>
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
        case tree: ImportOrExport => tree

        case tree if tree.symbol.is(Inline) =>
          val tpe = tree.symbol.info.asInstanceOf[ConstantType]
          cpy.Literal(tree)(tpe.value)

        // static object
        case tree: (Ident | Select | This) if isStaticObject(tree.symbol) =>
          getStaticObject(tree)(tree.symbol.moduleClass)

        // non-static object
        case tree: (Ident | Select | This) if isInaccessibleNonStaticObject(tree.symbol) =>
          val qualifier = getTransformedQualifier(tree)
          callMethod(tree)(qualifier, tree.symbol.asTerm, List.empty)

        // local variable
        case tree: Ident if isLocalVariable(tree.symbol) =>
          if tree.symbol.is(Lazy) then
            report.error(s"Evaluation of local lazy val not supported", tree.srcPos)
            tree
          else
            getCapturer(tree.symbol.asTerm) match
              case Some(capturer) =>
                if capturer.isClass then getClassCapture(tree)(tree.symbol, capturer.asClass)
                else getMethodCapture(tree)(tree.symbol, capturer.asTerm)
              case None => getLocalValue(tree)(tree.symbol)

        // assignement to local variable
        case tree @ Assign(lhs, _) if isLocalVariable(lhs.symbol) =>
          val variable = lhs.symbol.asTerm
          val rhs = transform(tree.rhs)
          getCapturer(variable) match
            case Some(capturer) =>
              if capturer.isClass then setClassCapture(tree)(variable, capturer.asClass, rhs)
              else setMethodCapture(tree)(variable, capturer.asTerm, rhs)
            case None => setLocalValue(tree)(variable, rhs)

        // inaccessible fields
        case tree: Select if isInaccessibleField(tree) =>
          if tree.symbol.is(JavaStatic) then getField(tree)(nullLiteral, tree.symbol.asTerm)
          else
            val qualifier = getTransformedQualifier(tree)
            getField(tree)(qualifier, tree.symbol.asTerm)

        // assignment to inaccessible fields
        case tree @ Assign(lhs, rhs) if isInaccessibleField(lhs) =>
          if lhs.symbol.is(JavaStatic) then setField(tree)(nullLiteral, lhs.symbol.asTerm, transform(rhs))
          else
            val qualifier = getTransformedQualifier(lhs)
            setField(tree)(qualifier, lhs.symbol.asTerm, transform(rhs))

        // this or outer this
        case tree @ This(Ident(name)) if !tree.symbol.is(Package) && !isOwnedByExpression(tree.symbol) =>
          thisOrOuterValue(tree)(tree.symbol.enclosingClass.asClass)

        // inaccessible constructors
        case tree: (Select | Apply | TypeApply) if isInaccessibleConstructor(tree) =>
          val args = getTransformedArgs(tree)
          val qualifier = getTransformedQualifierOfNew(tree)
          callConstructor(tree)(qualifier, tree.symbol.asTerm, args)

        // inaccessible methods
        case tree: (Ident | Select | Apply | TypeApply) if isInaccessibleMethod(tree) =>
          val args = getTransformedArgs(tree)
          if tree.symbol.is(JavaStatic) then callMethod(tree)(nullLiteral, tree.symbol.asTerm, args)
          else
            val qualifier = getTransformedQualifier(tree)
            callMethod(tree)(qualifier, tree.symbol.asTerm, args)

        case Typed(tree, tpt) if tpt.symbol.isType && !isTypeAccessible(tpt.symbol.asType) =>
          transform(tree)
        case tree =>
          super.transform(tree)

    /**
     * The symbol is a field and the expression class cannot access it
     * either because it is private or it belongs to an inacessible type
     */
    private def isInaccessibleField(tree: Tree)(using Context): Boolean =
      val symbol = tree.symbol
      symbol.isField
      && symbol.owner.isType
      && !isTermAccessible(symbol.asTerm, getQualifierTypeSymbol(tree))

    /**
     * The symbol is a real method and the expression class cannot access it
     * either because it is private or it belongs to an inaccessible type
     */
    private def isInaccessibleMethod(tree: Tree)(using Context): Boolean =
      val symbol = tree.symbol
      !isOwnedByExpression(symbol)
      && symbol.isRealMethod
      && (!symbol.owner.isType || !isTermAccessible(symbol.asTerm, getQualifierTypeSymbol(tree)))

    /**
     * The symbol is a constructor and the expression class cannot access it
     * either because it is an inaccessible method or it belong to a nested type (not static)
     */
    private def isInaccessibleConstructor(tree: Tree)(using Context): Boolean =
      val symbol = tree.symbol
      !isOwnedByExpression(symbol) &&
      symbol.isConstructor &&
      (isInaccessibleMethod(tree) || !symbol.owner.isStatic)

    private def getCapturer(variable: TermSymbol)(using Context): Option[Symbol] =
      // a local variable can be captured by a class or method
      val candidates = exprCtx.expressionSymbol.ownersIterator
        .takeWhile(_ != variable.owner)
        .filter(s => s.isClass || s.is(Method))
        .toSeq
      candidates
        .findLast(_.isClass)
        .orElse(candidates.find(_.is(Method)))

    private def getTransformedArgs(tree: Tree)(using Context): List[Tree] =
      tree match
        case _: (Ident | Select) => List.empty
        case Apply(fun, args) => getTransformedArgs(fun) ++ args.map(transform)
        case TypeApply(fun, _) => getTransformedArgs(fun)

    private def getQualifierTypeSymbol(tree: Tree)(using Context): TypeSymbol =
      tree match
        case Ident(_) => tree.symbol.enclosingClass.asClass
        case Select(qualifier, _) => qualifier.tpe.widenDealias.typeSymbol.asType
        case Apply(fun, _) => getQualifierTypeSymbol(fun)
        case TypeApply(fun, _) => getQualifierTypeSymbol(fun)

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
      def transformPrefix(prefix: Type): Tree =
        prefix match
          case NoPrefix =>
            thisOrOuterValue(typeTree)(typeTree.symbol.owner.enclosingClass.asClass)
          case prefix: ThisType =>
            thisOrOuterValue(typeTree)(prefix.cls)
          case ref: TermRef => transform(Ident(ref).withSpan(typeTree.span))
      def rec(tpe: Type): Tree =
        tpe match
          case TypeRef(prefix, _) => transformPrefix(prefix)
          case AppliedType(tycon, _) => rec(tycon)
      rec(typeTree.tpe)
  end ExpressionTransformer

  private def isExpressionVal(sym: Symbol)(using Context): Boolean =
    sym.name == exprCtx.expressionTermName

  // symbol can be a class or a method
  private def thisOrOuterValue(tree: Tree)(cls: ClassSymbol)(using Context): Tree =
    val ths = getThis(tree)(exprCtx.classOwners.head)
    val target = exprCtx.classOwners.indexOf(cls)
    if target >= 0 then
      exprCtx.classOwners
        .drop(1)
        .take(target)
        .foldLeft(ths) { (innerObj, outerSym) =>
          if innerObj == ths && exprCtx.localVariables.contains("$outer") then getLocalOuter(tree)(outerSym)
          else getOuter(tree)(innerObj, outerSym)
        }
    else nullLiteral

  private def getThis(tree: Tree)(cls: ClassSymbol)(using Context): Tree =
    reflectEval(tree)(
      nullLiteral,
      EvaluationStrategy.This(cls),
      List.empty,
      exprCtx.classOwners.head.typeRef
    )

  private def getLocalOuter(tree: Tree)(outerCls: ClassSymbol)(using Context): Tree =
    val strategy = EvaluationStrategy.LocalOuter(outerCls)
    reflectEval(tree)(nullLiteral, strategy, List.empty, outerCls.typeRef)

  private def getOuter(tree: Tree)(qualifier: Tree, outerCls: ClassSymbol)(using Context): Tree =
    val strategy = EvaluationStrategy.Outer(outerCls)
    reflectEval(tree)(qualifier, strategy, List.empty, outerCls.typeRef)

  private def getLocalValue(tree: Tree)(variable: Symbol)(using Context): Tree =
    val isByName = isByNameParam(variable.info)
    val strategy = EvaluationStrategy.LocalValue(variable.asTerm, isByName)
    reflectEval(tree)(nullLiteral, strategy, List.empty, tree.tpe)

  private def isByNameParam(tpe: Type)(using Context): Boolean =
    tpe match
      case _: ExprType => true
      case ref: TermRef => isByNameParam(ref.symbol.info)
      case _ => false

  private def setLocalValue(tree: Tree)(variable: Symbol, rhs: Tree)(using Context): Tree =
    val strategy = EvaluationStrategy.LocalValueAssign(variable.asTerm)
    reflectEval(tree)(nullLiteral, strategy, List(rhs), tree.tpe)

  private def getClassCapture(tree: Tree)(variable: Symbol, cls: ClassSymbol)(using Context): Tree =
    val byName = isByNameParam(variable.info)
    val strategy = EvaluationStrategy.ClassCapture(variable.asTerm, cls, byName)
    val qualifier = thisOrOuterValue(tree)(cls)
    reflectEval(tree)(qualifier, strategy, List.empty, tree.tpe)

  private def setClassCapture(tree: Tree)(variable: Symbol, cls: ClassSymbol, value: Tree)(using Context) =
    val strategy = EvaluationStrategy.ClassCaptureAssign(variable.asTerm, cls)
    val qualifier = thisOrOuterValue(tree)(cls)
    reflectEval(tree)(qualifier, strategy, List(value), tree.tpe)

  private def getMethodCapture(tree: Tree)(variable: Symbol, method: TermSymbol)(using Context): Tree =
    val isByName = isByNameParam(variable.info)
    val strategy =
      EvaluationStrategy.MethodCapture(variable.asTerm, method.asTerm, isByName)
    reflectEval(tree)(nullLiteral, strategy, List.empty, tree.tpe)

  private def setMethodCapture(tree: Tree)(variable: Symbol, method: Symbol, value: Tree)(using Context) =
    val strategy =
      EvaluationStrategy.MethodCaptureAssign(variable.asTerm, method.asTerm)
    reflectEval(tree)(nullLiteral, strategy, List(value), tree.tpe)

  private def getStaticObject(tree: Tree)(obj: Symbol)(using ctx: Context): Tree =
    val strategy = EvaluationStrategy.StaticObject(obj.asClass)
    reflectEval(tree)(nullLiteral, strategy, List.empty, obj.typeRef)

  private def getField(tree: Tree)(qualifier: Tree, field: TermSymbol)(using Context): Tree =
    val byName = isByNameParam(field.info)
    val strategy = EvaluationStrategy.Field(field, byName)
    reflectEval(tree)(qualifier, strategy, List.empty, tree.tpe)

  private def setField(tree: Tree)(qualifier: Tree, field: TermSymbol, rhs: Tree)(using Context): Tree =
    val strategy = EvaluationStrategy.FieldAssign(field)
    reflectEval(tree)(qualifier, strategy, List(rhs), tree.tpe)

  private def callMethod(tree: Tree)(qualifier: Tree, method: TermSymbol, args: List[Tree])(using Context): Tree =
    val strategy = EvaluationStrategy.MethodCall(method)
    reflectEval(tree)(qualifier, strategy, args, tree.tpe)

  private def callConstructor(tree: Tree)(qualifier: Tree, ctr: TermSymbol, args: List[Tree])(using Context): Tree =
    val strategy = EvaluationStrategy.ConstructorCall(ctr, ctr.owner.asClass)
    reflectEval(tree)(qualifier, strategy, args, tree.tpe)

  private def reflectEval(tree: Tree)(
      qualifier: Tree,
      strategy: EvaluationStrategy,
      args: List[Tree],
      tpe: Type
  )(using
      Context
  ): Tree =
    val reflectEval =
      cpy.Apply(tree)(
        Select(This(exprCtx.expressionClass), termName("reflectEval")),
        List(
          qualifier,
          Literal(Constant(strategy.toString)),
          JavaSeqLiteral(args, TypeTree(ctx.definitions.ObjectType))
        )
      )
    reflectEval.putAttachment(EvaluationStrategy, strategy)
    val widenDealiasTpe = tpe.widenDealias
    if isTypeAccessible(widenDealiasTpe.typeSymbol.asType)
    then reflectEval.cast(widenDealiasTpe)
    else reflectEval

  private def isStaticObject(symbol: Symbol)(using Context): Boolean =
    symbol.is(Module) &&
      symbol.isStatic &&
      !symbol.is(JavaDefined) &&
      !symbol.isRoot

  private def isInaccessibleNonStaticObject(symbol: Symbol)(using Context): Boolean =
    symbol.is(Module) &&
      !symbol.isStatic &&
      !symbol.isRoot &&
      !isOwnedByExpression(symbol)

  private def isLocalVariable(symbol: Symbol)(using Context): Boolean =
    !symbol.is(Method) && symbol.isLocalToBlock && !isOwnedByExpression(symbol)

  // Check if a term is accessible from the expression class
  private def isTermAccessible(symbol: TermSymbol, owner: TypeSymbol)(using
      Context
  ): Boolean =
    isOwnedByExpression(symbol)
      || (!symbol.isPrivate && !symbol.is(Protected) && isTypeAccessible(owner))

    // Check if a type is accessible from the expression class
  private def isTypeAccessible(symbol: TypeSymbol)(using Context): Boolean =
    isOwnedByExpression(symbol) || (
      !symbol.isLocal &&
        symbol.ownersIterator.forall(s => s.isPublic || s.privateWithin.is(PackageClass))
    )

  private def isOwnedByExpression(symbol: Symbol)(using Context): Boolean =
    val evaluateMethod = exprCtx.evaluateMethod
    symbol.ownersIterator.exists(_ == evaluateMethod)

object ExtractExpression:
  val name: String = "extract-expression"
