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
  def collect[S](sym: Symbol)(matcher: PartialFunction[LiftedTree[?], LiftedTree[S]])(using
      Context,
      Definitions
  ): Seq[LiftedTree[S]] =
    val collector = LiftedTreeCollector[S](sym, matcher)
    sym.tree.toSeq.flatMap(collector.collect)

class LiftedTreeCollector[S] private (root: Symbol, matcher: PartialFunction[LiftedTree[?], LiftedTree[S]])(using
    ctx: Context,
    defn: Definitions
):
  private val inlinedTrees = mutable.Map.empty[TermSymbol, Seq[LiftedTree[S]]]
  private var owner = root

  def collect(tree: Tree): Seq[LiftedTree[S]] =
    val buffer = mutable.Buffer.empty[LiftedTree[S]]

    object Traverser extends TreeTraverser:
      override def traverse(tree: Tree): Unit =
        // register lifted funs
        tree match
          case tree: DefDef if tree.symbol.isLocal => registerLiftedFun(LocalDef(tree))
          case tree: ValDef if tree.symbol.isLocal && tree.symbol.isModuleOrLazyVal =>
            registerLiftedFun(LocalLazyVal(tree))
          case tree: ClassDef if tree.symbol.isLocal => registerLiftedFun(LocalClass(tree))
          case tree: Lambda => registerLiftedFun(LambdaTree(tree))
          case tree: Try => registerLiftedFun(LiftedTry(owner, tree))
          case tree: Apply =>
            val isInline = tree.funSymbol.exists(_.isInline)
            // TODO remove try catch after next tasty-query version
            try
              tree.methodType.paramTypes.zip(tree.args).collect { case (byNameTpe: ByNameType, arg) =>
                registerLiftedFun(ByNameArg(owner, arg, byNameTpe.resultType, isInline))
              }
              for
                funSym <- tree.funSymbol
                if owner.isClass && funSym.isConstructor
                (paramTpe, arg) <- tree.methodType.paramTypes.zip(tree.args)
              do registerLiftedFun(ConstructorArg(owner.asClass, arg, paramTpe))
            catch case t => ()
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

      def registerLiftedFun(tree: LiftedTree[?]): Unit =
        matcher.lift(tree).foreach(buffer += _)
    end Traverser

    Traverser.traverse(tree)
    buffer.toSeq
  end collect

  private def collectInlineDef(symbol: TermSymbol): Seq[LiftedTree[S]] =
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
