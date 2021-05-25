package scala.tools.nsc

import scala.collection.mutable
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.transform.{Transform, TypingTransformers}

private[nsc] class EvalGlobal(settings: Settings, reporter: Reporter, val line: Int) extends Global(settings, reporter) {
  private var valDefsByName: Map[TermName, ValDef] = Map()
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
        case tree: ValDef =>
          if (tree.rhs.tpe.typeSymbol != NoSymbol) {
            valDefsByName += (tree.name -> tree)
          }
          tree
        case _ if tree.pos.line == line =>
          expression = tree
          tree
        case _ =>
          super.transform(tree)
      }
    }
  }

  class GenExpr extends Transform with TypingTransformers {

    import typer.typedPos

    override val global: EvalGlobal.this.type = EvalGlobal.this
    override val phaseName: String = "genexpr"
    override val runsAfter: List[String] = List()
    override val runsRightAfter: Option[String] = Some("extract")

    override protected def newTransformer(unit: CompilationUnit): Transformer = new GenExprTransformer(unit)

    class ExpressionTransformer(symbolsByName: Map[Name, Symbol]) extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case ident: Ident if symbolsByName.contains(ident.name) =>
          ident.setSymbol(symbolsByName(ident.name))
        case _ =>
          super.transform(tree)
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
      private var valuesByNameIdent: Ident = _

      override def transform(tree: Tree): Tree = tree match {
        case tree: Ident if tree.name == TermName("valuesByName") && valuesByNameIdent == null =>
          valuesByNameIdent = tree
          EmptyTree
        case DefDef(_, name, _, _, _, _) if name == TermName("evaluate") =>
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
            val newValDefsByName = valDefsByName.map { case (name, valDef) =>
              valDef.rhs match {
                case literal: Literal =>
                  newValDef(name, literal, tree.pos)
                case _ =>
                  val app = Apply(valuesByNameIdent, List(Literal(Constant(name.decode))))
                  val clazz = valDef.tpt.symbol.asInstanceOf[ClassSymbol]
                  val casted: gen.global.Tree = gen.mkCast(app, clazz.tpe)
                  newValDef(name, casted, tree.pos)
              }
            }

            // merge all symbols
            val symbolsByName = defFinder.symbolsByName.toMap ++ newValDefsByName.mapValues(_.symbol)

            // replace symbols in the expression with those from the `evaluate` method
            val newExpression = new ExpressionTransformer(symbolsByName).transform(expression)

            // create a new body
            val newRhs = new Block(block.stats ++ newValDefsByName.values, newExpression)

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

      private def newValDef(name: TermName, value: Tree, pos: Position) = {
        val tpe = valDefsByName(TermName(name.decode)).tpt.tpe
        val sym = NoSymbol.newTermSymbol(TermName(name.decode), pos).setInfo(tpe)
        val tt = TypeTree().setType(tpe)
        val newValDef = ValDef(Modifiers(), TermName(name.decode), tt, value).setSymbol(sym)
        name -> newValDef
      }
    }
  }
}
