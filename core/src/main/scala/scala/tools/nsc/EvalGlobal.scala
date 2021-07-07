package scala.tools.nsc

import scala.collection.mutable
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.transform.{Transform, TypingTransformers}

private[nsc] class EvalGlobal(
                               settings: Settings,
                               reporter: Reporter,
                               val line: Int,
                               valOrDefDefNames: Set[String]
                             ) extends Global(settings, reporter) {
  private var valOrDefDefs: Map[TermName, ValOrDefDef] = Map()
  private var expression: Tree = _

  override protected def computeInternalPhases(): Unit = {
    super.computeInternalPhases()

    addToPhasesSet(new Extract, "extract")
    addToPhasesSet(new GenExpr, "generate expression")
  }

  class Extract extends Transform {
    override val global: EvalGlobal.this.type = EvalGlobal.this
    override val phaseName: String = "extract"
    override val runsAfter: List[String] = List("delambdafy")
    override val runsRightAfter: Option[String] = None

    override protected def newTransformer(unit: CompilationUnit): Transformer = {
      if (unit.source.file.name == "<source>") new ExtractDefsTransformer
      else noopTransformer
    }

    class ExtractDefsTransformer extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case tree: ClassDef =>
          // Don't extract defs from the `Expression` class
          if (tree.name == TypeName("Expression")) tree
          else super.transform(tree)
        case tree: ValDef if valOrDefDefNames.contains(tree.name.decode) =>
          valOrDefDefs += (tree.name -> tree)
          super.transform(tree)
        case tree: DefDef if tree.symbol.isGetter && valOrDefDefNames.contains(tree.name.decode) =>
          valOrDefDefs += (tree.name -> tree)
          super.transform(tree)
        case _ if tree.pos.line == line =>
          expression = tree
          tree
        case _ =>
          super.transform(tree)
      }
    }
  }

  class GenExpr extends Transform with TypingTransformers {

    override val global: EvalGlobal.this.type = EvalGlobal.this
    override val phaseName: String = "genexpr"
    override val runsAfter: List[String] = List()
    override val runsRightAfter: Option[String] = Some("extract")

    override protected def newTransformer(unit: CompilationUnit): Transformer = new GenExprTransformer(unit)

    class ExpressionTransformer(symbolsByName: Map[Name, Symbol]) extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case tree: Ident =>
          val name = tree.name
          if (symbolsByName.contains(tree.name)) ident(name)
          else super.transform(tree)
        case tree: Apply if tree.fun.symbol.isGetter =>
          val name = tree.fun.asInstanceOf[Select].name
          if (symbolsByName.contains(name)) ident(name)
          else super.transform(tree)
        case _ =>
          super.transform(tree)
      }

      private def ident(name: Name) = {
        val symbol = symbolsByName(name)
        Ident(name)
          .setSymbol(symbol)
          .setType(symbol.tpe)
          .setPos(symbol.pos)
      }
    }

    class DefFinder extends Traverser {
      val symbolsByName: mutable.Map[Name, Symbol] = mutable.Map()

      override def traverse(tree: Tree): Unit = {
        tree match {
          case tree: ValDef =>
            symbolsByName += (tree.name -> tree.symbol)
          case _ =>
            traverseTrees(tree.children)
        }
      }
    }

    class GenExprTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

      import definitions._
      import typer._

      private var valuesByNameIdent: Ident = _

      override def transform(tree: Tree): Tree = tree match {
        case tree: Ident if tree.name == TermName("valuesByName") && valuesByNameIdent == null =>
          valuesByNameIdent = tree
          EmptyTree
        case tree: DefDef if tree.name == TermName("evaluate") =>
          // firstly, transform the body of the method
          super.transform(tree)

          var tpt: TypeTree = TypeTree()
          val derived = deriveDefDef(tree) { rhs =>
            // we can be sure that `rhs` is an instance of a `Block`
            val block = rhs.asInstanceOf[Block]

            // find all defs in the body of the `evaluate` method
            val defFinder = new DefFinder()
            defFinder.traverse(block)

            // replace original valDefs with synthesized valDefs with values that will be sent via JDI
            val newValOrDefDefs = valOrDefDefs.map { case (_, valOrDefDef) =>
              newValDef(tree, valOrDefDef)
            }

            // merge all symbols
            val symbolsByName = defFinder.symbolsByName.toMap ++ newValOrDefDefs.mapValues(_.symbol)

            // replace symbols in the expression with those from the `evaluate` method
            val newExpression = new ExpressionTransformer(symbolsByName).transform(expression)

            // create a new body
            val newRhs = new Block(block.stats ++ newValOrDefDefs.values, newExpression)

            tpt = TypeTree().copyAttrs(newExpression)
            typedPos(tree.pos)(newRhs).setType(tpt.tpe)
          }
          // update return type of the `evaluate` method
          derived
            .symbol
            .asInstanceOf[MethodSymbol]
            .modifyInfo(info => {
              val methodType = info.asInstanceOf[MethodType]
              methodType.copy(resultType = tpt.tpe)
            })
          derived.setType(tpt.tpe)
        case _ =>
          super.transform(tree)
      }

      private def newValDef(owner: DefDef, valOrDefDef: ValOrDefDef): (TermName, ValDef) = {
        val name = valOrDefDef.name
        val app = Apply(valuesByNameIdent, List(Literal(Constant(name.decode))))
        val tpt = valOrDefDef.tpt.asInstanceOf[TypeTree]
        if (tpt.symbol.isPrimitiveValueClass) {
          val unboxedMethodSym = currentRun.runDefinitions.unboxMethod(tpt.symbol)
          val casted = Apply(unboxedMethodSym, app)
          newValDef(owner, name, casted, valOrDefDef.pos)
        } else {
          val app = Apply(valuesByNameIdent, List(Literal(Constant(name.decode))))
          val tpt = valOrDefDef.tpt.asInstanceOf[TypeTree]
          val tapp = gen.mkTypeApply(gen.mkAttributedSelect(app, Object_asInstanceOf).asInstanceOf[Tree], List(tpt))
          val casted = Apply(tapp, Nil)
          newValDef(owner, name, casted, valOrDefDef.pos)
        }
      }

      private def newValDef(owner: DefDef, name: TermName, rhs: Tree, pos: Position): (TermName, ValDef) = {
        val tpt = valOrDefDefs(TermName(name.decode)).tpt
        val sym = owner.symbol.newValue(name.toTermName, pos).setInfo(tpt.tpe)
        val newValDef = ValDef(Modifiers(), TermName(name.decode), tpt, rhs).setSymbol(sym)
        name -> newValDef
      }
    }
  }
}
