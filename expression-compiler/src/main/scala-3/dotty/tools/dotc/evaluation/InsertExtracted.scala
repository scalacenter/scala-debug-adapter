package dotty.tools.dotc.evaluation

import dotty.tools.dotc.EvaluationContext
import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Symbols.TermSymbol
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.transform.SymUtils.isField
import dotty.tools.dotc.core.Names
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.ast.tpd

class InsertExtracted(using evalCtx: EvaluationContext) extends MiniPhase:
  override def phaseName: String = InsertExtracted.name

  override def transformDefDef(tree: DefDef)(using Context): Tree =
    if tree.name.toString == "evaluate" && tree.symbol.owner == evalCtx.expressionThis
    then
      evalCtx.evaluateMethod = tree.symbol
      val transformedExpression = ExpressionTransformer.transform(
        evalCtx.expressionValDef
      )
      val transformedNestedMethods = evalCtx.nestedMethods.values
        .map(nestedMethod => ExpressionTransformer.transform(nestedMethod))
        .toList
      val expressionBlock =
        Block(
          List(transformedExpression),
          evalCtx.expressionIdent
        )
      DefDef(
        tree.symbol.asInstanceOf[TermSymbol],
        List(),
        tree.tpt.tpe,
        Block(transformedNestedMethods, expressionBlock)
      )
    else super.transformDefDef(tree)

  object ExpressionTransformer extends TreeMap:
    override def transform(tree: Tree)(using Context): Tree =
      tree match
        case tree @ This(Ident(name)) =>
          val thisOrOuter =
            if tree.symbol == evalCtx.originalThis then "$this" else "$outer"
          if evalCtx.defTypes.contains(thisOrOuter) then mkIdent(thisOrOuter)
          else super.transform(tree)
        case tree @ Select(This(_), name)
            if tree.qualifier.symbol == evalCtx.originalThis && tree.symbol.isField =>
          mkIdent(name.toString)
        case Ident(name) if evalCtx.defTypes.contains(name.toString) =>
          mkIdent(name.toString)
        case tree: Apply
            if tree.fun.isInstanceOf[Select] && tree.fun.symbol.isPrivate =>
          val privateCall = mkCallPrivate(tree)
          super.transform(privateCall)
        case _ =>
          super.transform(tree)

  def mkIdent(name: String)(using Context) =
    val tree = Apply(
      Select(
        Select(This(evalCtx.expressionThis), Names.termName("valuesByName")),
        Names.termName("apply")
      ),
      List(Literal(Constant(name)))
    )

    val tpe = evalCtx.defTypes(name)
    TypeApply(
      Select(tree, Names.termName("asInstanceOf")),
      List(tpd.TypeTree(tpe))
    )

  def mkCallPrivate(tree: Apply)(using Context) =
    val fun = tree.fun.asInstanceOf[Select]
    val paramTypeNames = tree.args
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
      JavaSeqLiteral(tree.args, TypeTree(ctx.definitions.ObjectType))

    val app = Apply(
      Select(This(evalCtx.expressionThis), Names.termName("callPrivate")),
      List(
        fun.qualifier,
        Literal(Constant(tree.fun.asInstanceOf[Select].name.toString)),
        paramTypeNamesArray,
        argsArray
      )
    )
    app.cast(tree.tpe)

end InsertExtracted

object InsertExtracted:
  val name: String = "insert-extracted"
