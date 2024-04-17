package scala.tools.nsc.evaluation

import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.transform.InfoTransform

class ExtractExpression(override val global: ExpressionGlobal)
    extends InfoTransform
    with TypingTransformers
    with JavaEncoding {
  import global._

  override val phaseName: String = "extract-expression"
  override val runsAfter: List[String] = List("refchecks")
  override val runsRightAfter: Option[String] = None

  override def transformInfo(sym: Symbol, tpe: Type): Type = {
    // TODO what if the type is private
    if (isExpressionVal(sym.owner)) sym.owner = evaluateMethod
    tpe
  }

  override protected def newTransformer(unit: CompilationUnit): Transformer = new Transformer {
    var expressionTree: Tree = null
    override def transform(tree: Tree): Tree = tree match {
      case PackageDef(pid, stats) =>
        val evaluationClassDef =
          stats.find(_.symbol == expressionClass)
        val others = stats.filter(_.symbol != expressionClass)
        val transformedStats = (others ++ evaluationClassDef).map(transform)
        PackageDef(pid, transformedStats)
      case tree: ValDef if isExpressionVal(tree.symbol) =>
        expressionTree = tree.rhs
        storeExpression(tree.symbol)
        unitLiteral
      case tree: DefDef if tree.symbol == evaluateMethod =>
        val transformedExpr = new ExpressionTransformer(unit).transform(expressionTree)
        tree.copy(rhs = transformedExpr)
      case tree =>
        super.transform(tree)
    }
  }

  private class ExpressionTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    override def transform(tree: Tree): Tree = tree match {
      case tree: Import => tree

      case _: Ident | _: Select | _: This if isStaticObject(tree.symbol) =>
        getStaticObject(tree)(tree.symbol.moduleClass)

      case _: Ident | _: Select | _: This if isInaccessibleNonStaticObject(tree.symbol) =>
        val qualifier = getTransformedQualifier(tree)
        callMethod(tree)(qualifier, tree.symbol.asTerm, List.empty)

      case tree: Ident if isLocalVariable(tree.symbol) =>
        if (tree.symbol.isLazy) {
          // TODO try fix in Scala 2
          reporter.error(tree.pos, "Evaluation of local lazy val not supported")
          tree
        } else {
          getCapturer(tree.symbol.asTerm) match {
            case Some(capturer) =>
              if (capturer.isClass) getClassCapture(tree)(tree.symbol, capturer.asClass)
              else getMethodCapture(tree)(tree.symbol, capturer.asTerm)
            case None => getLocalValue(tree)(tree.symbol)
          }
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
        if (isJavaStatic(tree.symbol)) getField(tree)(nullLiteral, tree.symbol.asTerm)
        else {
          val qualifier = getTransformedQualifier(tree)
          getField(tree)(qualifier, tree.symbol.asTerm)
        }

      case tree @ Assign(lhs, rhs) if isInaccessibleField(lhs) =>
        if (isJavaStatic(tree.symbol)) setField(tree)(nullLiteral, lhs.symbol.asTerm, transform(rhs))
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
        if (isJavaStatic(tree.symbol)) callMethod(tree)(nullLiteral, tree.symbol.asTerm, args)
        else {
          val qualifier = getTransformedQualifier(tree)
          callMethod(tree)(qualifier, tree.symbol.asTerm, args)
        }

      case Typed(tree, tpt) if tpt.symbol.isType && !isTypeAccessible(tpt.symbol.asType) =>
        transform(tree)
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
      symbol.isMethod && !symbol.isAccessor && !symbol.isAnonymousFunction

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
      val candidates = expressionSymbol.ownersIterator
        .takeWhile(_ != variable.owner)
        .filter(s => s.isClass || s.isMethod)
        .toSeq
      candidates
        .reverseIterator
        .find(_.isClass)
        .orElse(candidates.find(_.isMethod))
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
        val classOwner = tree.symbol.enclClass.asClass
        if (isStaticObject(classOwner)) getStaticObject(tree)(classOwner)
        else thisOrOuterValue(tree)(classOwner)
      case Select(qualifier, _) =>
        val classOwner = tree.symbol.enclClass.asClass
        if (isStaticObject(classOwner)) getStaticObject(tree)(classOwner)
        else transform(qualifier)
      case Apply(fun, _) => getTransformedQualifier(fun)
      case TypeApply(fun, _) => getTransformedQualifier(fun)
    }

    private def getTransformedQualifierOfNew(tree: Tree): Tree = tree match {
      case Select(New(tpt), _) => getTransformedPrefix(tpt)
      case Apply(fun, _) => getTransformedQualifierOfNew(fun)
      case TypeApply(fun, _) => getTransformedQualifierOfNew(fun)
    }

    private def getTransformedPrefix(typeTree: Tree): Tree = {
      def transformPrefix(prefix: Type): Tree = prefix match {
        case NoPrefix =>
          thisOrOuterValue(typeTree)(typeTree.symbol.owner.enclClass.asClass)
        case prefix: ThisType =>
          thisOrOuterValue(typeTree)(prefix.sym.asClass)
        case SingleType(pre, sym) => transform(Select(transformPrefix(pre), sym))
      }
      typeTree.tpe match {
        case TypeRef(prefix, _, _) => transformPrefix(prefix)
      }
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
      else nullLiteral
    }

    private def getThis(tree: Tree)(cls: ClassSymbol): Tree =
      reflectEval(tree)(nullLiteral, EvaluationStrategy.This(cls), List.empty, classOwners.head.toType)

    private def getLocalOuter(tree: Tree)(outerCls: ClassSymbol): Tree = {
      val strategy = EvaluationStrategy.LocalOuter(outerCls)
      reflectEval(tree)(nullLiteral, strategy, List.empty, outerCls.tpe)
    }

    private def getOuter(tree: Tree)(qualifier: Tree, outerCls: ClassSymbol): Tree = {
      val strategy = EvaluationStrategy.Outer(outerCls)
      reflectEval(tree)(qualifier, strategy, List.empty, outerCls.tpe)
    }

    private def getLocalValue(tree: Tree)(variable: Symbol): Tree = {
      val isByName = isByNameParam(variable.info)
      val strategy = EvaluationStrategy.LocalValue(variable.asTerm, isByName)
      reflectEval(tree)(nullLiteral, strategy, List.empty, tree.tpe)
    }

    private def isByNameParam(tpe: Type): Boolean = tpe.typeSymbol == definitions.ByNameParamClass

    private def setLocalValue(tree: Tree)(variable: Symbol, rhs: Tree): Tree = {
      val strategy = EvaluationStrategy.LocalValueAssign(variable.asTerm)
      reflectEval(tree)(nullLiteral, strategy, List(rhs), tree.tpe)
    }

    private def getClassCapture(tree: Tree)(variable: Symbol, cls: ClassSymbol): Tree = {
      val byName = isByNameParam(variable.info)
      val strategy = EvaluationStrategy.ClassCapture(variable.asTerm, cls, byName)
      val qualifier = thisOrOuterValue(tree)(cls)
      reflectEval(tree)(qualifier, strategy, List.empty, tree.tpe)
    }

    private def setClassCapture(tree: Tree)(variable: Symbol, cls: ClassSymbol, value: Tree) = {
      val strategy = EvaluationStrategy.ClassCaptureAssign(variable.asTerm, cls)
      val qualifier = thisOrOuterValue(tree)(cls)
      reflectEval(tree)(qualifier, strategy, List(value), tree.tpe)
    }

    private def getMethodCapture(tree: Tree)(variable: Symbol, method: TermSymbol): Tree = {
      val isByName = isByNameParam(variable.info)
      val strategy =
        EvaluationStrategy.MethodCapture(variable.asTerm, method.asTerm, isByName)
      reflectEval(tree)(nullLiteral, strategy, List.empty, tree.tpe)
    }

    private def setMethodCapture(tree: Tree)(variable: Symbol, method: Symbol, value: Tree) = {
      val strategy =
        EvaluationStrategy.MethodCaptureAssign(variable.asTerm, method.asTerm)
      reflectEval(tree)(nullLiteral, strategy, List(value), tree.tpe)
    }

    private def getStaticObject(tree: Tree)(obj: Symbol): Tree = {
      val strategy = EvaluationStrategy.StaticObject(obj.asClass)
      reflectEval(tree)(nullLiteral, strategy, List.empty, obj.tpe)
    }

    private def getField(tree: Tree)(qualifier: Tree, field: TermSymbol): Tree = {
      val byName = isByNameParam(field.info)
      val strategy = EvaluationStrategy.Field(field, byName)
      reflectEval(tree)(qualifier, strategy, List.empty, tree.tpe)
    }

    private def setField(tree: Tree)(qualifier: Tree, field: TermSymbol, rhs: Tree): Tree = {
      val strategy = EvaluationStrategy.FieldAssign(field)
      reflectEval(tree)(qualifier, strategy, List(rhs), tree.tpe)
    }

    private def callMethod(tree: Tree)(qualifier: Tree, method: TermSymbol, args: List[Tree]): Tree = {
      val strategy = EvaluationStrategy.MethodCall(method)
      reflectEval(tree)(qualifier, strategy, args, tree.tpe)
    }

    private def callConstructor(tree: Tree)(qualifier: Tree, ctr: TermSymbol, args: List[Tree]): Tree = {
      val strategy = EvaluationStrategy.ConstructorCall(ctr, ctr.owner.asClass)
      reflectEval(tree)(qualifier, strategy, args, tree.tpe)
    }

    private def reflectEval(
        tree: Tree
    )(qualifier: Tree, strategy: EvaluationStrategy, args: List[Tree], tpe: Type): Tree = {
      val attachments = tree.attachments.addElement(strategy)
      val reflectEval = treeCopy
        .Apply(
          tree,
          fun = Select(This(expressionClass), TermName("reflectEval")),
          args = List(
            qualifier,
            Literal(Constant(strategy.toString)),
            ArrayValue(TypeTree(definitions.ObjectTpe), args)
          )
        )
        .setAttachments(attachments)
      val widenDealiasTpe = tpe.dealiasWiden
      if (isTypeAccessible(widenDealiasTpe.typeSymbol.asType)) gen.mkAsInstanceOf(reflectEval, widenDealiasTpe)
      else reflectEval
    }
  }

  private def isExpressionVal(sym: Symbol): Boolean = sym.name == expressionTermName

  private def isJavaStatic(sym: Symbol): Boolean = sym.isJava && sym.isStatic

  private def isStaticObject(symbol: Symbol): Boolean =
    symbol.isModule && symbol.isStatic && !symbol.isJava && !symbol.isRoot

  private def isInaccessibleNonStaticObject(symbol: Symbol): Boolean =
    symbol.isModule && !symbol.isStatic && !symbol.isRoot && !isOwnedByExpression(symbol)

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
    symbol.ownersIterator.exists(_ == evaluateMethod)
}
