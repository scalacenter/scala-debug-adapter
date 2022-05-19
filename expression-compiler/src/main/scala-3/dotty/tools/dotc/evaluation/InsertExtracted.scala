package dotty.tools.dotc.evaluation

import dotty.tools.dotc.EvaluationContext
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.NameKinds
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Phases.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.transform.SymUtils.*
import dotty.tools.dotc.core.Types.MethodType

class InsertExtracted(using evalCtx: EvaluationContext) extends MiniPhase:
  override def phaseName: String = InsertExtracted.name

  override def transformDefDef(tree: DefDef)(using Context): Tree =
    if tree.name == evalCtx.evaluateName && tree.symbol.owner == evalCtx.expressionClass
    then
      val transformedExpression =
        ExpressionTransformer.transform(evalCtx.expressionTree)
      val transformedNestedMethods = evalCtx.nestedMethods.values
        .map(nestedMethod => ExpressionTransformer.transform(nestedMethod))
        .toList
      DefDef(
        tree.symbol.asInstanceOf[TermSymbol],
        List(),
        tree.tpt.tpe,
        Block(transformedNestedMethods, transformedExpression)
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
          val qualifier = getLocalValue("$this", evalCtx.originalThis.thisType)
          callMethod(qualifier, tree.symbol.asTerm, List.empty, tree.tpe)
        case tree: Select if isNonStaticObject(tree.symbol) =>
          val qualifier = transform(tree.qualifier)
          callMethod(qualifier, tree.symbol.asTerm, List.empty, tree.tpe)

        // local value
        case tree @ Ident(name) if isLocalVal(tree.symbol) =>
          getLocalValue(name.toString, tree.tpe)

        // inaccessible fields
        case tree @ Ident(name) if isInaccessibleField(tree.symbol) =>
          val qualifier =
            if isStaticObject(tree.symbol.owner)
            then getStaticObject(tree.symbol.owner)
            else getLocalValue("$this", evalCtx.originalThis.thisType)
          getPrivateField(qualifier, tree.symbol.asTerm, tree.tpe)
        case tree @ Select(qualifier, name)
            if isInaccessibleField(tree.symbol) =>
          val qualifier = transform(tree.qualifier)
          getPrivateField(qualifier, tree.symbol.asTerm, tree.tpe)

        // this or outer this
        case tree @ This(Ident(name)) =>
          val innerThis = getLocalValue("$this", evalCtx.originalThis.thisType)
          val owners =
            evalCtx.originalThis.ownersIterator.filter(_.isClass).toSeq
          val target = owners.indexOf(tree.symbol)
          owners
            .take(target + 1)
            .drop(1)
            .foldLeft(innerThis) { (innerObj, outerSym) =>
              getOuter(innerObj, outerSym.thisType)
            }

        // inaccessible constructors
        case tree @ Apply(fun: Select, _)
            if isInaccessibleConstructor(fun.symbol) =>
          val args = tree.args.map(transform)
          callConstructor(fun.symbol.asTerm, args, tree.tpe)

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
            else getLocalValue("$this", owner.info)
          val args = tree.args.map(transform)
          callMethod(qualifier, fun.symbol.asTerm, args, tree.tpe)

        case tree => super.transform(tree)

  private def getLocalValue(name: String, tpe: Type)(using Context) =
    val tree = Apply(
      Select(
        Select(This(evalCtx.expressionClass), termName("valuesByName")),
        termName("apply")
      ),
      List(Literal(Constant(name)))
    )
    cast(tree, tpe)

  private def getOuter(qualifier: Tree, tpe: Type)(using
      Context
  ): Tree =
    val tree = Apply(
      Select(This(evalCtx.expressionClass), termName("getOuter")),
      List(qualifier)
    )
    cast(tree, tpe)

  private def getPrivateField(qualifier: Tree, field: TermSymbol, tpe: Type)(
      using Context
  ): Tree =
    val tree =
      Apply(
        Select(This(evalCtx.expressionClass), termName("getPrivateField")),
        List(
          qualifier,
          Literal(Constant(field.name.toString)),
          Literal(Constant(field.expandedName.toString))
        )
      )
    cast(tree, tpe)

  private def getStaticObject(obj: Symbol)(using ctx: Context): Tree =
    assert(obj.isClass)
    val className = atPhase(genBCodePhase)(JavaEncoding.encode(obj))
    Apply(
      Select(This(evalCtx.expressionClass), termName("getStaticObject")),
      List(Literal(Constant(className)))
    )

  private def callMethod(
      qualifier: Tree,
      fun: TermSymbol,
      args: List[Tree],
      tpe: Type
  )(using Context): Tree =
    val (paramTypesNames, returnTypeName) = atPhase(genBCodePhase) {
      val methodType = fun.info.asInstanceOf[MethodType]
      (
        methodType.paramInfos.map(JavaEncoding.encode),
        JavaEncoding.encode(methodType.resType)
      )
    }
    val paramTypesArray = JavaSeqLiteral(
      paramTypesNames.map(t => Literal(Constant(t))),
      TypeTree(ctx.definitions.StringType)
    )
    val argsArray =
      JavaSeqLiteral(args, TypeTree(ctx.definitions.ObjectType))

    val tree = Apply(
      Select(This(evalCtx.expressionClass), termName("callMethod")),
      List(
        qualifier,
        Literal(Constant(fun.name.toString)),
        paramTypesArray,
        Literal(Constant(returnTypeName)),
        argsArray
      )
    )
    cast(tree, tpe)

  private def callConstructor(
      ctr: TermSymbol,
      args: List[Tree],
      tpe: Type
  )(using Context): Tree =
    val (paramTypesNames, clazzName) = atPhase(genBCodePhase) {
      val methodType = ctr.info.asInstanceOf[MethodType]
      (
        methodType.paramInfos.map(JavaEncoding.encode),
        JavaEncoding.encode(methodType.resType)
      )
    }
    val paramTypesArray = JavaSeqLiteral(
      paramTypesNames.map(t => Literal(Constant(t))),
      TypeTree(ctx.definitions.StringType)
    )
    val argsArray =
      JavaSeqLiteral(args, TypeTree(ctx.definitions.ObjectType))

    val tree = Apply(
      Select(This(evalCtx.expressionClass), termName("callConstructor")),
      List(
        Literal(Constant(clazzName)),
        paramTypesArray,
        argsArray
      )
    )
    cast(tree, tpe)

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
    symbol.isRealMethod && symbol.owner.isType && !isTermAccessible(
      symbol.asTerm,
      symbol.owner.asType
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
      symbol.ownersIterator.forall(_ != evalCtx.expressionSymbol) &&
      evalCtx.expressionOwners.contains(symbol.maybeOwner)

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

object InsertExtracted:
  val name: String = "insert-extracted"
