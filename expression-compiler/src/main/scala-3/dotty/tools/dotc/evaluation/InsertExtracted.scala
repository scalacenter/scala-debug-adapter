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
    if tree.name.toString == "evaluate" && tree.symbol.owner == evalCtx.expressionClass
    then
      evalCtx.evaluateMethod = tree.symbol
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
          getStaticObject(tree.symbol)
        case tree: Select if isStaticObject(tree.symbol) =>
          getStaticObject(tree.symbol)

        // non-static object
        case tree: Ident if isNonStaticObject(tree.symbol) =>
          val qualifier = getLocalValue("$this", evalCtx.originalThis.thisType)
          callPrivateMethod(qualifier, tree.symbol.asTerm, List.empty, tree.tpe)
        case tree: Select if isNonStaticObject(tree.symbol) =>
          val qualifier = transform(tree.qualifier)
          callPrivateMethod(qualifier, tree.symbol.asTerm, List.empty, tree.tpe)

        // local value
        case tree @ Ident(name) if isLocalVal(tree.symbol) =>
          getLocalValue(name.toString, tree.tpe)

        // private field
        case tree @ Ident(name) if isPrivateField(tree.symbol) =>
          val qualifier =
            if isStaticObject(tree.symbol.owner)
            then getStaticObject(tree.symbol.owner)
            else getLocalValue("$this", evalCtx.originalThis.thisType)
          getPrivateField(qualifier, tree.symbol.asTerm, tree.tpe)
        case tree @ Select(qualifier, name) if isPrivateField(tree.symbol) =>
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

        // private method
        case tree @ Apply(fun: Select, _) if isPrivateMethod(fun.symbol) =>
          val args = tree.args.map(transform)
          if fun.symbol.isClassConstructor
          then callPrivateConstructor(fun.symbol.asTerm, args, tree.tpe)
          else
            val qualifier = transform(fun.qualifier)
            callPrivateMethod(qualifier, fun.symbol.asTerm, args, tree.tpe)
        case tree @ Apply(fun: Ident, _) if isPrivateMethod(fun.symbol) =>
          val owner = fun.symbol.owner
          val qualifier =
            if isStaticObject(owner)
            then getStaticObject(owner)
            else getLocalValue("$this", owner.info)
          val args = tree.args.map(transform)
          callPrivateMethod(qualifier, fun.symbol.asTerm, args, tree.tpe)

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
    val packageClass = obj.enclosingPackageClass
    val flatName = obj.flatName.toString
    val className = if flatName.endsWith("$") then flatName else flatName + "$"
    val fullName =
      if packageClass.isEmptyPackage
      then className
      else s"${packageClass.fullName}.$className"
    Apply(
      Select(This(evalCtx.expressionClass), termName("getStaticObject")),
      List(Literal(Constant(fullName)))
    )

  private def callPrivateMethod(
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
      Select(This(evalCtx.expressionClass), termName("callPrivateMethod")),
      List(
        qualifier,
        Literal(Constant(fun.name.toString)),
        paramTypesArray,
        Literal(Constant(returnTypeName)),
        argsArray
      )
    )
    cast(tree, tpe)

  private def callPrivateConstructor(
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
      Select(This(evalCtx.expressionClass), termName("callPrivateConstructor")),
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

  private def isPrivateField(symbol: Symbol)(using Context): Boolean =
    symbol.isField && symbol.owner.isType && !isTermAccessible(
      symbol.asTerm,
      symbol.owner.asType
    )

  private def isPrivateMethod(symbol: Symbol)(using Context): Boolean =
    symbol.isRealMethod && symbol.owner.isType && !isTermAccessible(
      symbol.asTerm,
      symbol.owner.asType
    )

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
    symbol.isAccessibleFrom(evalCtx.expressionClass.thisType) && !symbol.isLocal

object InsertExtracted:
  val name: String = "insert-extracted"
