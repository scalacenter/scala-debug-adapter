package scala.tools.nsc.evaluation

import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.transform.Transform
import scala.collection.mutable.Buffer

class ExtractExpression(override val global: ExpressionGlobal)
    extends Transform
    with TypingTransformers
    with JavaEncoding {
  import global._

  override val phaseName: String = "extract-expression"
  override val runsAfter: List[String] = List("uncurry")
  override val runsBefore: List[String] = List("fields")
  override val runsRightAfter: Option[String] = None

  override protected def newTransformer(unit: CompilationUnit): Transformer = new Transformer {
    var expressionTree: Tree = null
    override def transform(tree: Tree): Tree = tree match {
      case packageDef @ PackageDef(_, stats) =>
        val evaluationClassDef = stats.find(_.symbol == expressionClass)
        val others = stats.filter(_.symbol != expressionClass)
        val transformedStats = (others ++ evaluationClassDef).map(transform)
        packageDef.copy(stats = transformedStats).copyAttrs(packageDef)
      case tree: ValDef if isExpressionVal(tree.symbol) =>
        expressionTree = tree.rhs
        storeExpression(tree.symbol)
        gen.mkLiteralUnit.setType(definitions.UnitTpe)
      case tree: DefDef if tree.symbol == evaluate =>
        val transformer = new ExpressionTransformer(unit)
        val transformedExpr = transformer.transform(expressionTree)
        transformer.localDefs.foreach(_.owner = evaluate)
        tree.copy(rhs = transformedExpr).copyAttrs(tree)
      case tree =>
        super.transform(tree)
    }
  }

  private class ExpressionTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    val localDefs = Buffer.empty[Symbol]
    override def transform(tree: Tree): Tree = tree match {
      case tree: Import => tree

      case _: Ident | _: Select | _: This if isStaticObject(tree.symbol) =>
        getStaticObject(tree)(tree.symbol)

      case _: This | _: Apply if isInaccessibleNonStaticObject(tree.symbol) =>
        val qualifier = getTransformedQualifier(tree)
        callMethod(tree)(qualifier, tree.symbol.asTerm, List.empty)

      case tree: Ident if isLocalVariable(tree.symbol) =>
        getCapturer(tree.symbol.asTerm) match {
          case Some(capturer) =>
            if (capturer.isClass) getClassCapture(tree)(tree.symbol, capturer.asClass)
            else getMethodCapture(tree)(tree.symbol, capturer.asTerm)
          case None => getLocalValue(tree)(tree.symbol)
        }

      case tree @ Assign(lhs, _) if isLocalVariable(lhs.symbol) =>
        val variable = lhs.symbol.asTerm
        val rhs = transform(tree.rhs)
        getCapturer(variable) match {
          case Some(capturer) =>
            if (capturer.isClass) setClassCapture(tree)(variable, capturer.asClass, rhs)
            else setMethodCapture(tree)(variable, capturer.asTerm, rhs)
          case None => setLocalValue(tree)(variable, rhs)
        }

      case tree: Select if isInaccessibleField(tree) =>
        if (isJavaStatic(tree.symbol)) getField(tree)(mkNullLiteral, tree.symbol.asTerm)
        else {
          val qualifier = getTransformedQualifier(tree)
          getField(tree)(qualifier, tree.symbol.asTerm)
        }

      case tree @ Assign(lhs, rhs) if isInaccessibleField(lhs) =>
        if (isJavaStatic(lhs.symbol)) setField(tree)(mkNullLiteral, lhs.symbol.asTerm, transform(rhs))
        else {
          val qualifier = getTransformedQualifier(lhs)
          setField(tree)(qualifier, lhs.symbol.asTerm, transform(rhs))
        }

      case This(name) if !tree.symbol.hasPackageFlag && !isOwnedByExpression(tree.symbol) =>
        thisOrOuterValue(tree)(tree.symbol.enclClass.asClass)

      case _: Select | _: Apply | _: TypeApply if isInaccessibleConstructor(tree) =>
        val args = getTransformedArgs(tree)
        val qualifier = getTransformedQualifierOfNew(tree)
        callConstructor(tree)(qualifier, tree.symbol.asTerm, args)

      case _: Ident | _: Select | _: Apply | _: TypeApply if isInaccessibleMethod(tree) =>
        val args = getTransformedArgs(tree)
        if (isJavaStatic(tree.symbol)) callMethod(tree)(mkNullLiteral, tree.symbol.asTerm, args)
        else {
          val qualifier = getTransformedQualifier(tree)
          callMethod(tree)(qualifier, tree.symbol.asTerm, args)
        }

      case Typed(tree, tpt) if tpt.symbol.isType && !isTypeAccessible(tpt.symbol.asType) =>
        transform(tree)

      case tree @ (_: DefTree | _: Function) if tree.symbol.owner == expressionVal =>
        localDefs += tree.symbol
        if (tree.symbol.isModule) localDefs += tree.symbol.moduleClass
        super.transform(tree)

      case tree =>
        super.transform(tree)
    }

    /**
     * The symbol is a field and the expression class cannot access it
     * either because it is private or it belongs to an inaccessible type
     */
    private def isInaccessibleField(tree: Tree): Boolean = {
      val symbol = tree.symbol
      symbol.isField &&
      symbol.owner.isType &&
      !isTermAccessible(symbol.asTerm, getQualifierTypeSymbol(tree))
    }

    /**
     * The symbol is a real method and the expression class cannot access it
     * either because it is private or it belongs to an inaccessible type
     */
    private def isInaccessibleMethod(tree: Tree): Boolean = {
      val symbol = tree.symbol
      !isOwnedByExpression(symbol) &&
      isRealMethod(symbol) &&
      (!symbol.owner.isType || !isTermAccessible(symbol.asTerm, getQualifierTypeSymbol(tree)))
    }

    private def isRealMethod(symbol: Symbol): Boolean =
      symbol.isMethod && !symbol.isAnonymousFunction

    /**
     * The symbol is a constructor and the expression class cannot access it
     * either because it is an inaccessible method or it belong to a nested type (not static)
     */
    private def isInaccessibleConstructor(tree: Tree): Boolean = {
      val symbol = tree.symbol
      !isOwnedByExpression(symbol) &&
      symbol.isConstructor &&
      (isInaccessibleMethod(tree) || !symbol.owner.isStatic)
    }

    private def getCapturer(variable: TermSymbol): Option[Symbol] = {
      // a local variable can be captured by a class or method
      val candidates = expressionVal.ownersIterator
        .takeWhile(_ != variable.owner)
        .filter(s => s.isClass || s.isMethod)
        .toSeq
      candidates.reverseIterator
        .find(_.isClass)
        .orElse(candidates.find(s => s.isMethod))
    }

    private def getTransformedArgs(tree: Tree): List[Tree] = tree match {
      case _: Ident | _: Select => List.empty
      case Apply(fun, args) => getTransformedArgs(fun) ++ args.map(transform)
      case TypeApply(fun, _) => getTransformedArgs(fun)
    }

    private def getQualifierTypeSymbol(tree: Tree): TypeSymbol = tree match {
      case Ident(_) => tree.symbol.enclClass.asClass
      case Select(qualifier, _) => qualifier.tpe.dealiasWiden.typeSymbol.asType
      case Apply(fun, _) => getQualifierTypeSymbol(fun)
      case TypeApply(fun, _) => getQualifierTypeSymbol(fun)
    }

    private def getTransformedQualifier(tree: Tree): Tree = tree match {
      case Ident(_) =>
        // it is a local method, it captures its outer value
        thisOrOuterValue(tree)(tree.symbol.enclClass.asClass)
      case Select(qualifier, _) => transform(qualifier)
      case Apply(fun, _) => getTransformedQualifier(fun)
      case TypeApply(fun, _) => getTransformedQualifier(fun)
    }

    private def getTransformedQualifierOfNew(tree: Tree): Tree = tree match {
      case Select(New(tpt), _) =>
        tpt.tpe.prefix match {
          case NoPrefix =>
            // it's a local class, it captures its outer value
            thisOrOuterValue(tree)(tpt.symbol.owner.enclClass.asClass)
          case prefix => transform(reifyPrefix(prefix))
        }
      case Apply(fun, _) => getTransformedQualifierOfNew(fun)
      case TypeApply(fun, _) => getTransformedQualifierOfNew(fun)
    }

    private def reifyPrefix(prefix: Type): Tree = prefix match {
      case prefix: ThisType => gen.mkAttributedThis(prefix.sym)
      case SingleType(ThisType(pre), sym) if pre.isPackageClass => gen.mkAttributedIdent(sym)
      case SingleType(NoPrefix, sym) => gen.mkAttributedIdent(sym)
      case SingleType(pre, sym) => gen.mkAttributedSelect(reifyPrefix(pre), sym)
    }

    // symbol can be a class or a method
    private def thisOrOuterValue(tree: Tree)(cls: ClassSymbol): Tree = {
      val ths = getThis(tree)(classOwners.head)
      val target = classOwners.indexOf(cls)
      if (target >= 0)
        classOwners
          .drop(1)
          .take(target)
          .foldLeft(ths) { (innerObj, outerSym) =>
            if (innerObj == ths && localVariables.contains("$outer")) getLocalOuter(tree)(outerSym)
            else getOuter(tree)(innerObj, outerSym)
          }
      else mkNullLiteral
    }

    private def getThis(tree: Tree)(cls: ClassSymbol): Tree =
      callReflectEval(tree)(mkNullLiteral, EvaluationStrategy.This(cls), List.empty, classOwners.head.toType)

    private def getLocalOuter(tree: Tree)(outerCls: ClassSymbol): Tree = {
      val strategy = EvaluationStrategy.LocalOuter(outerCls)
      callReflectEval(tree)(mkNullLiteral, strategy, List.empty, outerCls.tpe)
    }

    private def getOuter(tree: Tree)(qualifier: Tree, outerCls: ClassSymbol): Tree = {
      val strategy = EvaluationStrategy.Outer(outerCls)
      callReflectEval(tree)(qualifier, strategy, List.empty, outerCls.tpe)
    }

    private def getLocalValue(tree: Tree)(variable: Symbol): Tree = {
      val isByName = isByNameParam(variable.info)
      val strategy = EvaluationStrategy.LocalValue(variable.asTerm, isByName)
      callReflectEval(tree)(mkNullLiteral, strategy, List.empty, tree.tpe)
    }

    private def isByNameParam(tpe: Type): Boolean = tpe.typeSymbol == definitions.ByNameParamClass

    private def setLocalValue(tree: Tree)(variable: Symbol, rhs: Tree): Tree = {
      val strategy = EvaluationStrategy.LocalValueAssign(variable.asTerm)
      callReflectEval(tree)(mkNullLiteral, strategy, List(rhs), tree.tpe)
    }

    private def getClassCapture(tree: Tree)(variable: Symbol, cls: ClassSymbol): Tree = {
      val byName = isByNameParam(variable.info)
      val strategy = EvaluationStrategy.ClassCapture(variable.asTerm, cls, byName)
      val qualifier = thisOrOuterValue(tree)(cls)
      callReflectEval(tree)(qualifier, strategy, List.empty, tree.tpe)
    }

    private def setClassCapture(tree: Tree)(variable: Symbol, cls: ClassSymbol, value: Tree) = {
      val strategy = EvaluationStrategy.ClassCaptureAssign(variable.asTerm, cls)
      val qualifier = thisOrOuterValue(tree)(cls)
      callReflectEval(tree)(qualifier, strategy, List(value), tree.tpe)
    }

    private def getMethodCapture(tree: Tree)(variable: Symbol, method: TermSymbol): Tree = {
      val isByName = isByNameParam(variable.info)
      val strategy =
        EvaluationStrategy.MethodCapture(variable.asTerm, method.asTerm, isByName)
      callReflectEval(tree)(mkNullLiteral, strategy, List.empty, tree.tpe)
    }

    private def setMethodCapture(tree: Tree)(variable: Symbol, method: Symbol, value: Tree) = {
      val strategy = EvaluationStrategy.MethodCaptureAssign(variable.asTerm, method.asTerm)
      callReflectEval(tree)(mkNullLiteral, strategy, List(value), tree.tpe)
    }

    private def getStaticObject(tree: Tree)(obj: Symbol): Tree = {
      val cls = if (obj.isClass) obj.asClass else obj.moduleClass.asClass
      val strategy = EvaluationStrategy.StaticObject(cls)
      callReflectEval(tree)(mkNullLiteral, strategy, List.empty, obj.tpe)
    }

    private def getField(tree: Tree)(qualifier: Tree, field: TermSymbol): Tree = {
      val byName = isByNameParam(field.info)
      val strategy = EvaluationStrategy.Field(field, byName)
      callReflectEval(tree)(qualifier, strategy, List.empty, tree.tpe.resultType)
    }

    private def setField(tree: Tree)(qualifier: Tree, field: TermSymbol, rhs: Tree): Tree = {
      val strategy = EvaluationStrategy.FieldAssign(field)
      callReflectEval(tree)(qualifier, strategy, List(rhs), tree.tpe)
    }

    private def callMethod(tree: Tree)(qualifier: Tree, method: TermSymbol, args: List[Tree]): Tree = {
      val strategy = EvaluationStrategy.MethodCall(method)
      callReflectEval(tree)(qualifier, strategy, args, tree.tpe)
    }

    private def callConstructor(tree: Tree)(qualifier: Tree, ctr: TermSymbol, args: List[Tree]): Tree = {
      val strategy = EvaluationStrategy.ConstructorCall(ctr, ctr.owner.asClass)
      callReflectEval(tree)(qualifier, strategy, args, tree.tpe)
    }

    private def callReflectEval(
        tree: Tree
    )(qualifier: Tree, strategy: EvaluationStrategy, args: List[Tree], tpe: Type): Tree = {
      val attachments = tree.attachments.addElement(strategy)
      val methodCall = gen
        .mkMethodCall(
          reflectEval,
          List(qualifier, mkStringLiteral(strategy.toString), mkObjectArray(args))
        )
        .setType(definitions.AnyTpe)
        .setPos(tree.pos)
        .setAttachments(attachments)
      val widenDealiasTpe = tpe.dealiasWiden
      if (isTypeAccessible(widenDealiasTpe.typeSymbol.asType)) {
        gen.mkCast(methodCall, widenDealiasTpe).setType(widenDealiasTpe)
      } else methodCall
    }
  }

  private def isExpressionVal(sym: Symbol): Boolean = sym.name == expressionTermName

  private def isJavaStatic(sym: Symbol): Boolean = sym.isJava && sym.isStatic

  private def isStaticObject(symbol: Symbol): Boolean =
    symbol.isModuleOrModuleClass && symbol.isStatic && !symbol.isJava && !symbol.isRoot

  private def isInaccessibleNonStaticObject(symbol: Symbol): Boolean =
    symbol.isModuleOrModuleClass && !symbol.isStatic && !symbol.isRoot && !isOwnedByExpression(symbol)

  private def isLocalVariable(symbol: Symbol): Boolean =
    !symbol.isMethod && symbol.isLocalToBlock && !isOwnedByExpression(symbol)

  // Check if a term is accessible from the expression class
  private def isTermAccessible(symbol: TermSymbol, owner: TypeSymbol): Boolean =
    isOwnedByExpression(symbol) ||
      (!symbol.isPrivate && !symbol.isProtected && isTypeAccessible(owner))

  // Check if a type is accessible from the expression class
  private def isTypeAccessible(symbol: TypeSymbol): Boolean =
    isOwnedByExpression(symbol) || (
      !symbol.isLocalToBlock &&
        symbol.ownersIterator.forall(s => s.isPublic || s.privateWithin.isPackageClass)
    )

  private def isOwnedByExpression(symbol: Symbol): Boolean =
    symbol.ownersIterator.exists(_ == expressionVal)
}
