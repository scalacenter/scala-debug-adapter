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

/**
 * Collect all trees that could be lifted by the compiler: local defs, lambdas, try clauses, by-name applications
 * Recurse on inline methods. Remember the inline method applications to later substitute the type params,
 * and compute the capture.
 */
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
