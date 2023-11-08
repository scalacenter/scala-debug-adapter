package ch.epfl.scala.debugadapter.internal.stacktrace

import tastyquery.Trees.*
import tastyquery.Symbols.*
import tastyquery.Types.*
import tastyquery.Contexts.*
import tastyquery.debugadapter.Substituters

case class InlineMethodApply private (termRefTree: TermReferenceTree, typeArgs: List[Type], args: Seq[TermTree]):
  def symbol(using Context): TermSymbol = termRefTree.symbol.asTerm

  def substTypeParams(tpe: TypeOrMethodic)(using Context): TypeOrMethodic =
    val typeParamSymbols = symbol.tree.toList
      .collect { case tree: DefDef => tree.paramLists }
      .flatten
      .collect { case Right(typeParams) => typeParams }
      .flatten
      .map(_.symbol)
      .collect { case sym: LocalTypeParamSymbol => sym }
    Substituters.substLocalTypeParams(tpe, typeParamSymbols, typeArgs)

object InlineMethodApply:
  def unapply(tree: Tree)(using Context): Option[InlineMethodApply] =
    def rec(tree: Tree, typeArgsAcc: List[Type], argsAcc: Seq[TermTree]): Option[InlineMethodApply] =
      tree match
        case termTree: TermReferenceTree if termTree.symbol.isInline && termTree.symbol.asTerm.isMethod =>
          Some(InlineMethodApply(termTree, typeArgsAcc, argsAcc))
        case Apply(fun, args) => rec(fun, typeArgsAcc, args ++ argsAcc)
        case TypeApply(fun, typeArgs) => rec(fun, typeArgs.map(_.toType) ++ typeArgsAcc, argsAcc)
        case _ => None
    tree match
      case tree: TermTree if !tree.tpe.isInstanceOf[MethodicType] => rec(tree, List.empty, Seq.empty)
      case _ => None
