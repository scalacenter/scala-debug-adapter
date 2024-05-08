package ch.epfl.scala.debugadapter.internal.stacktrace

import tastyquery.Trees.*
import ch.epfl.scala.debugadapter.internal.binary.SourceLines
import scala.collection.mutable
import tastyquery.Symbols.*
import tastyquery.Traversers.*
import tastyquery.Contexts.*
import tastyquery.SourcePosition
import tastyquery.Types.*
import tastyquery.Traversers
import tastyquery.Exceptions.NonMethodReferenceException

/**
 * Collect all trees that could be lifted by the compiler: local defs, lambdas, try clauses, by-name applications
 * Recurse on inline methods. Remember the inline method applications to later substitute the type params,
 * and compute the capture.
 */
object LiftedTreeCollector:
  def collect(sym: Symbol)(using Context, Definitions, ThrowOrWarn): Seq[LiftedTree[?]] =
    val collector = LiftedTreeCollector(sym)
    sym.tree.toSeq.flatMap(collector.collect)

class LiftedTreeCollector private (root: Symbol)(using Context, Definitions, ThrowOrWarn):
  private val inlinedTrees = mutable.Map.empty[TermSymbol, Seq[LiftedTree[?]]]
  private var owner = root

  def collect(tree: Tree): Seq[LiftedTree[?]] =
    val buffer = mutable.Buffer.empty[LiftedTree[?]]

    object Traverser extends TreeTraverser:
      override def traverse(tree: Tree): Unit =
        // register lifted funs
        tree match
          case tree: DefDef if tree.symbol.isLocal => buffer += LocalDef(tree)
          case tree: ValDef if tree.symbol.isLocal && tree.symbol.isModuleOrLazyVal =>
            buffer += LocalLazyVal(tree)
          case tree: ClassDef if tree.symbol.isLocal => buffer += LocalClass(tree)
          case tree: Lambda => buffer += LambdaTree(tree)
          case tree: Try => buffer += LiftedTry(owner, tree)
          case tree: Apply =>
            for
              symbol <- tree.safeFunSymbol
              methodType <- tree.safeMethodType
            do
              val paramTypesAndArgs = methodType.paramTypes.zip(tree.args)
              for case (byNameTpe: ByNameType, arg) <- paramTypesAndArgs do
                buffer += ByNameArg(owner, arg, byNameTpe.resultType, symbol.isInline)
              if owner.isClass && symbol.isConstructor then
                for (paramTpe, arg) <- paramTypesAndArgs do buffer += ConstructorArg(owner.asClass, arg, paramTpe)
          case _ => ()

        // recurse
        tree match
          case tree: DefTree =>
            if tree.symbol.isInline then ()
            else
              val previousOwner = owner
              owner = tree.symbol
              super.traverse(tree)
              owner = previousOwner
          case InlineCall(inlineCall) =>
            val liftedTrees = inlinedTrees.getOrElseUpdate(inlineCall.symbol, collectInlineDef(inlineCall.symbol))
            buffer ++= liftedTrees.map(InlinedFromDef(_, inlineCall))
            buffer ++= inlineCall.args.flatMap { arg =>
              extractLambda(arg) match
                case Some(lambda) =>
                  val params = lambda.meth.symbol.asTerm.paramSymbols
                  collect(arg).map(InlinedFromArg(_, params, inlineCall.args))
                case None => collect(arg)
            }
            super.traverse(inlineCall.termRefTree)
          case tree: (StatementTree | Template | CaseDef) => super.traverse(tree)
          case _ => ()

    end Traverser

    Traverser.traverse(tree)
    buffer.toSeq
  end collect

  private def collectInlineDef(symbol: TermSymbol): Seq[LiftedTree[?]] =
    inlinedTrees(symbol) = Seq.empty // break recursion
    symbol.tree.flatMap(extractRHS).toSeq.flatMap(collect)

  private def extractLambda(tree: StatementTree): Option[Lambda] =
    tree match
      case lambda: Lambda => Some(lambda)
      case block: Block => extractLambda(block.expr)
      case _ => None

  private def extractRHS(tree: DefTree): Option[TermTree] =
    tree match
      case tree: DefDef => tree.rhs
      case _ => None
