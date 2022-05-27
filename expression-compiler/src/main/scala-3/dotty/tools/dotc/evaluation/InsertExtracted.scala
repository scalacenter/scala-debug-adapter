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
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.transform.SymUtils.*
import dotty.tools.dotc.core.DenotTransformers.DenotTransformer
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.SymDenotations.SymDenotation

class InsertExtracted(using evalCtx: EvaluationContext) extends MiniPhase with DenotTransformer:
  override def phaseName: String = InsertExtracted.name

  /**
   * Change the return type of the `evaluate` method
   * and to update the owner and types of the symDenotations inserted into `evaluate`.
   */
  override def transform(ref: SingleDenotation)(using
      Context
  ): SingleDenotation =
    ref match
      case ref: SymDenotation
          if ref.name == evalCtx.evaluateName && ref.owner.name.toString == evalCtx.expressionClassName =>
        // set return type of the `evaluate` method to the return type of the expression
        // this is only useful to avoid boxing of primitive types
        if evalCtx.expressionType.typeSymbol.isPrimitiveValueClass then
          val info = MethodType(Nil)(_ => Nil, _ => evalCtx.expressionType)
          ref.copySymDenotation(info = info)
        else ref
      case ref: SymDenotation if ref.maybeOwner == evalCtx.expressionSymbol =>
        // update owner of the symDenotation, e.g. local vals
        // after it was inserted to `evaluate` method
        ref.copySymDenotation(owner = evalCtx.evaluateMethod)
      case ref: SymDenotation if evalCtx.nestedMethods.contains(ref) =>
        // update owner and type of the nested method
        val info = ref.info.asInstanceOf[MethodType]
        val paramsInfos =
          info.paramInfos.map { info =>
            if isTypeAccessible(info.typeSymbol.asType) then info
            else defn.ObjectType
          }
        val resType =
          if isTypeAccessible(info.resType.typeSymbol.asType) then info.resType
          else defn.ObjectType

        ref.copySymDenotation(
          owner = evalCtx.evaluateMethod,
          info = MethodType(info.paramNames)(_ => paramsInfos, _ => resType)
        )
      case _ =>
        ref

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
          callMethod(thisValue, tree.symbol.asTerm, List.empty, tree.tpe)
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
            else thisValue
          getPrivateField(qualifier, tree.symbol.asTerm, tree.tpe)
        case tree @ Select(qualifier, name)
            if isInaccessibleField(tree.symbol) =>
          val qualifier = transform(tree.qualifier)
          getPrivateField(qualifier, tree.symbol.asTerm, tree.tpe)

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
            case tree @ Ident(_) => thisOrOuterValue(tree.symbol)
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
            else thisValue
          val args = tree.args.map(transform)
          callMethod(qualifier, fun.symbol.asTerm, args, tree.tpe)

        case tree => super.transform(tree)

  private def thisOrOuterValue(symbol: Symbol)(using Context): Tree =
    val owners = evalCtx.originalThis.ownersIterator.filter(_.isClass).toSeq
    val target = owners.indexOf(symbol)
    owners
      .take(target + 1)
      .drop(1)
      .foldLeft(thisValue) { (innerObj, outerSym) =>
        getOuter(innerObj, outerSym.thisType)
      }

  private def thisValue(using Context): Tree =
    getLocalValue("$this", evalCtx.originalThis.thisType)

  private def getLocalValue(name: String, tpe: Type)(using Context): Tree =
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
      qualifier: Tree,
      ctr: TermSymbol,
      args: List[Tree],
      tpe: Type
  )(using Context): Tree =
    val (paramNames, paramTypesNames, clazzName) = atPhase(genBCodePhase) {
      val methodType = ctr.info.asInstanceOf[MethodType]
      (
        methodType.paramNames.map(_.toString),
        methodType.paramInfos.map(JavaEncoding.encode),
        JavaEncoding.encode(methodType.resType)
      )
    }
    val capturedArgs =
      paramNames.dropRight(args.size).map {
        case "$outer" => qualifier
        case other =>
          // we should probably fail here
          getLocalValue(other, defn.ObjectType)
      }

    val paramTypesArray = JavaSeqLiteral(
      paramTypesNames.map(t => Literal(Constant(t))),
      TypeTree(ctx.definitions.StringType)
    )
    val argsArray =
      JavaSeqLiteral(capturedArgs ++ args, TypeTree(ctx.definitions.ObjectType))

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
      evalCtx.expressionOwners.contains(symbol.owner)

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
