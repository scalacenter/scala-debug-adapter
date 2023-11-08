package ch.epfl.scala.debugadapter.internal.stacktrace

import tastyquery.Symbols.*
import tastyquery.Traversers.*
import tastyquery.Trees.*
import tastyquery.Contexts.*

object Capturer:
  def collect(sym: TermSymbol)(using Context): Set[String] =
    val collector = new Capturer(Some(sym))
    val treeOpt =
      if sym.isLocal && sym.isModuleVal then sym.moduleClass.flatMap(_.tree)
      else sym.tree
    treeOpt.foreach(collector.traverse)
    collector.variables

  def collect(tree: TermTree)(using Context): Set[String] =
    val collector = new Capturer(None)
    collector.traverse(tree)
    collector.variables

  def collect(trees: Seq[TermTree])(using Context): Set[String] =
    val collector = new Capturer(None)
    trees.foreach(collector.traverse)
    collector.variables

class Capturer private (sym: Option[TermSymbol])(using Context) extends TreeTraverser:
  var variables = Set.empty[String]
  var alreadySeen = sym.toSet
  override def traverse(tree: Tree): Unit =
    tree match
      case _: TypeTree => ()
      case ident: Ident =>
        ident.symbol match
          case sym: TermSymbol =>
            variables += sym.name.toString
            if sym.isLocal && (sym.isMethod || sym.isLazyVal || sym.isModuleVal) && !alreadySeen.contains(sym) then
              alreadySeen += sym
              val treeOpt =
                if sym.isModuleVal then sym.moduleClass.flatMap(_.tree)
                else sym.tree
              treeOpt.foreach(traverse)
          case _ => ()
      case _ => super.traverse(tree)
