package ch.epfl.scala.debugadapter.internal.stacktrace

import tastyquery.Trees.*
import tastyquery.Symbols.*
import tastyquery.Types.*
import tastyquery.Contexts.*
import tastyquery.debugadapter.Substituters

case class InlineMethodApply private (
    termRefTree: TermReferenceTree,
    typeArgs: List[Type],
    args: Seq[TermTree],
    inliningTree: Tree
):
  def symbol(using Context): TermSymbol = termRefTree.symbol.asTerm

  def substTypeParams(tpe: TermType)(using Context): TermType =
    Substituters.substLocalTypeParams(tpe, symbol.typeParamSymbols, typeArgs)

  def paramsMap(using Context): Map[TermSymbol, TermTree] =
    symbol.paramSymbols.zip(args).toMap

  def paramTypes(using Context): Seq[Type] =
    symbol.declaredType.allParamTypes

object InlineMethodApply:
  def unapply(fullTree: Tree)(using Context): Option[InlineMethodApply] =
    def rec(tree: Tree, typeArgsAcc: List[Type], argsAcc: Seq[TermTree]): Option[InlineMethodApply] =
      tree match
        case termTree: TermReferenceTree if termTree.symbol.isInline && termTree.symbol.asTerm.isMethod =>
          Some(InlineMethodApply(termTree, typeArgsAcc, argsAcc, fullTree))
        case Apply(fun, args) => rec(fun, typeArgsAcc, args ++ argsAcc)
        case TypeApply(fun, typeArgs) => rec(fun, typeArgs.map(_.toType) ++ typeArgsAcc, argsAcc)
        case _ => None
    // TODO remove the try catch after the next TASTy Query release
    try
      fullTree match
        case tree: TermTree if !tree.tpe.isInstanceOf[MethodicType] => rec(tree, List.empty, Seq.empty)
        case _ => None
    catch case t => None
