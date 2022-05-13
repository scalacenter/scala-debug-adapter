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
import dotty.tools.dotc.transform.SymUtils.isField

class InsertExtracted(using evalCtx: EvaluationContext) extends MiniPhase:
  override def phaseName: String = InsertExtracted.name

  override def transformDefDef(tree: DefDef)(using Context): Tree =
    if tree.name.toString == "evaluate" && tree.symbol.owner == evalCtx.expressionThis
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
      super.transform(tree) match
        // static object
        case tree: Ident if isStaticObject(tree.symbol) =>
          getStaticObject(tree.symbol)
        case tree: Select if isStaticObject(tree.symbol) =>
          getStaticObject(tree.symbol)

        // private field
        case tree @ Ident(name) if isPrivateField(tree.symbol) =>
          getPrivateField(mkIdent("$this"), name, tree.tpe)
        case tree @ Select(qualifier, name) if isPrivateField(tree.symbol) =>
          getPrivateField(tree.qualifier, tree.name, tree.tpe)

        case tree @ This(Ident(name)) =>
          val thisOrOuter =
            if tree.symbol == evalCtx.originalThis then "$this" else "$outer"
          if evalCtx.defTypes.contains(thisOrOuter) then mkIdent(thisOrOuter)
          else tree
        case Ident(name) if evalCtx.defTypes.contains(name.toString) =>
          mkIdent(name.toString)

        // private methods
        case tree @ Apply(fun @ Select(qualifier, name), args)
            if isPrivateMethod(fun.symbol) =>
          callPrivateMethod(qualifier, name, args, tree.tpe)
        case tree @ Apply(fun @ Ident(name), args)
            if isPrivateMethod(fun.symbol) =>
          if fun.symbol.owner == evalCtx.originalThis
          then callPrivateMethod(mkIdent("$this"), name, args, tree.tpe)
          else tree

        case tree => tree

  def mkIdent(name: String)(using Context) =
    val tree = Apply(
      Select(
        Select(This(evalCtx.expressionThis), termName("valuesByName")),
        termName("apply")
      ),
      List(Literal(Constant(name)))
    )

    val tpe = evalCtx.defTypes(name)
    TypeApply(
      Select(tree, termName("asInstanceOf")),
      List(TypeTree(tpe.widen))
    )

  private def getPrivateField(qualifier: Tree, name: Name, tpe: Type)(using
      Context
  ): Tree =
    val app =
      Apply(
        Select(This(evalCtx.expressionThis), termName("getPrivateField")),
        List(
          qualifier,
          Literal(Constant(name.toString))
        )
      )
    app.cast(tpe.widen)

  private def getStaticObject(symbol: Symbol)(using ctx: Context): Tree =
    val packageClass = symbol.enclosingPackageClass
    val name =
      if packageClass.isEmptyPackage
      then symbol.flatName.toString + "$"
      else s"${packageClass.fullName}.${symbol.flatName}$$"
    Apply(
      Select(This(evalCtx.expressionThis), termName("getStaticObject")),
      List(Literal(Constant(name)))
    )

  private def callPrivateMethod(
      qualifier: Tree,
      name: Name,
      args: List[Tree],
      tpe: Type
  )(using Context): Tree =
    val paramTypeNames = args
      .map(arg =>
        val tpeSymbol = arg.tpe.typeSymbol
        val paramTypeName =
          if (tpeSymbol.isPrimitiveValueClass) then
            tpeSymbol.fullName.toString.stripPrefix("scala.").toLowerCase()
          else tpeSymbol.fullName.toString
        Literal(Constant(paramTypeName))
      )
    val paramTypeNamesArray =
      JavaSeqLiteral(paramTypeNames, TypeTree(ctx.definitions.StringType))
    val argsArray =
      JavaSeqLiteral(args, TypeTree(ctx.definitions.ObjectType))

    val app = Apply(
      Select(This(evalCtx.expressionThis), termName("callPrivateMethod")),
      List(
        qualifier,
        Literal(Constant(name.toString)),
        paramTypeNamesArray,
        argsArray
      )
    )
    app.cast(tpe)

  private def isStaticObject(symbol: Symbol)(using Context): Boolean =
    symbol.is(Module) && symbol.isStatic && !symbol.isRoot

  private def isPrivateField(symbol: Symbol)(using Context): Boolean =
    symbol.isField && symbol.isPrivate

  private def isPrivateMethod(symbol: Symbol)(using Context): Boolean =
    symbol.isRealMethod && (symbol.isPrivate || symbol.owner.isPrivate)

end InsertExtracted

object InsertExtracted:
  val name: String = "insert-extracted"
