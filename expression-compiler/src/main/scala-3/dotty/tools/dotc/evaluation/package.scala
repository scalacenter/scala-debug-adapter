package dotty.tools.dotc.evaluation

import dotty.tools.dotc.EvaluationContext
import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.Symbols.ClassSymbol
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.util.Property

val ExpressionProperty = new Property.Key[Unit]

def mkIdent(name: String)(using evalCtx: EvaluationContext)(using Context) =
  val tree = Apply(
    Select(
      Select(This(evalCtx.expressionThis), termName("valuesByName")),
      termName("apply")
    ),
    List(Literal(Constant(name)))
  )
  mkCast(tree, evalCtx.defTypes(name))

private def mkCast(tree: Tree, tpe: Type)(using Context): Tree =
  TypeApply(
    Select(tree, termName("asInstanceOf")),
    List(Ident(tpe.typeSymbol.namedType))
  )

def mkCallPrivate(tree: Apply)(using evalCtx: EvaluationContext)(using
    Context
) =
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
    Select(This(evalCtx.expressionThis), termName("callPrivate")),
    List(
      fun.qualifier,
      Literal(Constant(tree.fun.asInstanceOf[Select].name.toString)),
      paramTypeNamesArray,
      argsArray
    )
  )
  app.cast(tree.tpe)
