package ch.epfl.scala.debugadapter.internal.stacktrace

import tastyquery.Trees.*
import tastyquery.Symbols.*
import tastyquery.Types.*
import tastyquery.Contexts.*

case class InlineMethodApply private (termRefTree: TermReferenceTree, symbol: TermSymbol, typeArgs: Seq[Type], args: Seq[Tree])
object InlineMethodApply:
  def unapply(tree: Tree)(using Context): Option[InlineMethodApply] =
    def rec(tree: Tree, typeArgsAcc: Seq[Type], argsAcc: Seq[Tree]): Option[InlineMethodApply] =
      tree match
        case termTree: TermReferenceTree if termTree.symbol.isInline && termTree.symbol.asTerm.isMethod =>
          Some(InlineMethodApply(termTree, termTree.symbol.asTerm, typeArgsAcc, argsAcc))
        case Apply(fun, args) => rec(fun, typeArgsAcc, args ++ argsAcc)
        case TypeApply(fun, typeArgs) => rec(fun, typeArgs.map(_.toType) ++ typeArgsAcc, argsAcc)
        case _ => None
    tree match
      case tree: TermTree if !tree.tpe.isInstanceOf[MethodicType] => rec(tree, Seq.empty, Seq.empty)
      case _ => None
      
