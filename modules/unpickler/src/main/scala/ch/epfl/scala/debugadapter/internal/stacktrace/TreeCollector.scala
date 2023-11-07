package ch.epfl.scala.debugadapter.internal.stacktrace

import tastyquery.Trees.*
import ch.epfl.scala.debugadapter.internal.binary.SourceLines
import scala.collection.mutable
import tastyquery.Symbols.*
import tastyquery.Traversers.*
import tastyquery.Contexts.*
import tastyquery.SourcePosition
import ch.epfl.scala.debugadapter.internal.stacktrace.Inlined
import tastyquery.Types.*

case class Inlined[T](value: T, inlinedFrom: List[InlineMethodApply])

object Inlined:
  extension (tree: Inlined[Tree])
    def pos: SourcePosition = tree.value.pos

  extension [T] (xs: Inlined[Seq[T]])
    def traverse: Seq[Inlined[T]] =
      xs.value.map(Inlined(_, xs.inlinedFrom))

  def lift[A, B](pf: PartialFunction[A, B]): PartialFunction[Inlined[A], Inlined[B]] =
    def f(inlined: Inlined[A]): Option[Inlined[B]] =
      pf.lift(inlined.value).map(Inlined(_, inlined.inlinedFrom))
    f.unlift

class TreeCollector(using Context):
  private val inlinedTrees = mutable.Map.empty[TermSymbol, Seq[Inlined[Tree]]]

  def collect(tree: Tree, sourceLines: Option[SourceLines]): Seq[Inlined[Tree]] =
    val buffer = mutable.Buffer.empty[Inlined[Tree]]

    def inlineTree(tree: Tree): Seq[Inlined[Tree]] =
      tree match
        case InlineMethodApply(inlineMethodApply) =>
          def collectInline = inlineMethodApply.symbol.tree.map(collect(_, None)).getOrElse(Seq.empty)
          val collectedTrees = inlinedTrees.getOrElseUpdate(inlineMethodApply.symbol, collectInline)
          collectedTrees.map(tree => tree.copy(inlinedFrom = inlineMethodApply +: tree.inlinedFrom))
        case _ => Seq.empty

    object Traverser extends TreeTraverser:
      override def traverse(tree: Tree): Unit =
        if sourceLines.forall(tree.matchLines) then
          // add tree to the buffer
          tree match
            case tree: DefTree if tree.symbol.isLocal =>
              buffer += Inlined(tree, Nil)
            case tree: (Lambda | Try) =>
              buffer += Inlined(tree, Nil)
            case tree: Apply =>
              tree.fun.tpe.widenTermRef match
                case tpe: MethodType if tpe.paramTypes.exists(_.isInstanceOf[ByNameType]) =>
                  buffer += Inlined(tree, Nil)
                case _ => ()
            case _ => ()
            // add the inline trees to the buffer, if the current tree is an inline method apply
          buffer ++= inlineTree(tree)
          // recurse
          tree match
            case tree: (StatementTree | Template | CaseDef) => super.traverse(tree)
            case _ => ()
        else
          // bug in dotty: wrong pos of `def $new` in the companion object of an enum
          // the pos is outside the companion object, in the enum
          tree match
            case ClassDef(_, template, sym) if sym.companionClass.exists(_.isEnum) => super.traverse(template.body)
            case _ => ()
    Traverser.traverse(tree)
    buffer.toSeq
  end collect

