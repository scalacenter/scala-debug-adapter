package ch.epfl.scala.debugadapter.internal.stacktrace

import tastyquery.Trees.*
import ch.epfl.scala.debugadapter.internal.binary.SourceLines
import scala.collection.mutable
import tastyquery.Symbols.*
import tastyquery.Traversers.*
import tastyquery.Contexts.*
import tastyquery.SourcePosition
import ch.epfl.scala.debugadapter.internal.stacktrace.LiftedFun
import tastyquery.Types.*

case class LiftedFun[T](value: T, inlinedFrom: List[InlineMethodApply], inlineCapture: Set[String]):
  def isInline: Boolean = inlinedFrom.nonEmpty

object LiftedFun:
  extension (tree: LiftedFun[Tree]) def pos: SourcePosition = tree.value.pos

  extension [T](xs: LiftedFun[Seq[T]])
    def traverse: Seq[LiftedFun[T]] =
      xs.value.map(LiftedFun(_, xs.inlinedFrom, xs.inlineCapture))

  def lift[A, B](pf: PartialFunction[A, B]): PartialFunction[LiftedFun[A], LiftedFun[B]] =
    def f(inlined: LiftedFun[A]): Option[LiftedFun[B]] =
      pf.lift(inlined.value).map(LiftedFun(_, inlined.inlinedFrom, inlined.inlineCapture))
    f.unlift

object LiftedFunCollector:
  def collect(tree: Tree, sourceLines: Option[SourceLines])(using Context): Seq[LiftedFun[Tree]] =
    val collector = new LiftedFunCollector()
    collector.collect(tree, sourceLines, Set.empty)

class LiftedFunCollector private (using Context):
  private val inlinedTrees = mutable.Map.empty[TermSymbol, Seq[LiftedFun[Tree]]]

  def collect(tree: Tree, sourceLines: Option[SourceLines], inlineCapture: Set[String]): Seq[LiftedFun[Tree]] =
    val buffer = mutable.Buffer.empty[LiftedFun[Tree]]

    object Traverser extends TreeTraverser:
      override def traverse(tree: Tree): Unit =
        if sourceLines.forall(tree.matchLines) then
          // add tree to the buffer
          tree match
            case tree: DefTree if tree.symbol.isLocal =>
              buffer += LiftedFun(tree, Nil, inlineCapture)
            case tree: (Lambda | Try) =>
              buffer += LiftedFun(tree, Nil, inlineCapture)
            case tree: Apply =>
              tree.fun.tpe.widenTermRef match
                case tpe: MethodType if tpe.paramTypes.exists(_.isInstanceOf[ByNameType]) =>
                  buffer += LiftedFun(tree, Nil, inlineCapture)
                case _ => ()
            case _ => ()

          // recurse on inline methods and child nodes
          tree match
            case InlineMethodApply(inlineMethodApply) =>
              def collectInline =
                inlineMethodApply.symbol.tree.map(collect(_, None, inlineCapture)).getOrElse(Seq.empty)
              val collectedTrees = inlinedTrees.getOrElseUpdate(inlineMethodApply.symbol, collectInline)
              buffer ++= collectedTrees.map(tree => tree.copy(inlinedFrom = inlineMethodApply +: tree.inlinedFrom))
              val newCapture = Capturer.collect(inlineMethodApply.args)
              // TODO: should inject newCapture only if the arg is an inline function
              buffer ++= inlineMethodApply.args.flatMap(collect(_, sourceLines, inlineCapture ++ newCapture))
              super.traverse(inlineMethodApply.termRefTree)
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
