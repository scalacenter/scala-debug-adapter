package scala.tools.nsc.evaluation

import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.transform.{Transform, TypingTransformers}

class InsertExpression(override val global: ExpressionGlobal) extends Transform with TypingTransformers {
  import global._

  override val phaseName: String = "insert-expression"
  override val runsAfter: List[String] = List("parser")
  override val runsRightAfter: Option[String] = None

  override protected def newTransformer(
      unit: CompilationUnit
  ): Transformer = new Inserter()

  /**
   * This transformer:
   * - inserts the expression at the line of the breakpoint, which allows us to extract all needed defs, including implicit ones too.
   * Thanks to that, evaluation of expression is done in the context of a breakpoint and logic behaves the same way like the
   * normal code would behave.
   * - inserts the expression class in the same package as the expression. This allows us to call package private methods without any trouble.
   */
  private class Inserter extends Transformer {
    private val expressionClassSource =
      s"""class $expressionClassName(thisObject: Any, names: Array[String], values: Array[Object]) {
         |  val valuesByName = names.reverse.map(_.asInstanceOf[String]).zip(values.reverse).toMap
         |
         |  def evaluate() = {
         |    ()
         |  }
         |}
         |
         |object $expressionClassName {
         |  def callPrivateMethod(obj: Any, methodName: String, paramTypeNames: Array[String], args: Array[Object]) = {
         |    val methods = obj.getClass.getDeclaredMethods
         |    val method = methods
         |      .find { m =>
         |        m.getName == methodName && m.getParameterTypes.map(_.getName).toSeq == paramTypeNames.toSeq
         |      }
         |      .get
         |    method.setAccessible(true)
         |    method.invoke(obj, args: _*)
         |  }
         |}
         |""".stripMargin

    private val parsedExpression = parseExpression(expression)
    private val parsedExpressionClassAndObject =
      parseExpressionClassAndObject(
        expressionClassSource
      )

    private var expressionInserted = false

    private def parseExpression(expression: String): Tree = {
      // It needs to be wrapped because it's not possible to parse single expression.
      // `$expression` is wrapped in a block to support multiline expressions.
      val wrappedExpressionSource =
        s"""object Expression {
           |  {
           |    $expression
           |  }
           |}
           |""".stripMargin
      val parsedWrappedExpression =
        parse("<wrapped-expression>", wrappedExpressionSource)
          .asInstanceOf[PackageDef]
      val parsed = parsedWrappedExpression.stats.head
        .asInstanceOf[ModuleDef]
        .impl
        .body
        .last
        .setPos(NoPosition)
      parsed match {
        case df: ValOrDefDef =>
          Block(df, Literal(Constant(())))
        case expr => expr
      }
    }

    private def parseExpressionClassAndObject(source: String): Seq[Tree] = {
      val parsedExpressionClass =
        parse("<expression>", source).asInstanceOf[PackageDef]
      parsedExpressionClass.stats match {
        case cls :: obj :: _ =>
          cls.setPos(NoPosition)
          obj.setPos(NoPosition)
          Seq(cls, obj)
        case stats =>
          throw new IllegalArgumentException(
            s"Expected at least two statements but got ${stats.size}"
          )
      }
    }

    private def parse(sourceName: String, source: String): Tree = {
      newUnitParser(
        new CompilationUnit(new BatchSourceFile(sourceName, source))
      ).parse()
    }

    private def filterOutTailRec(defdef: DefDef): DefDef = {
      val copiedMods = defdef.mods.copy(
        annotations = defdef.mods.annotations.filterNot {
          case Apply(
                Select(
                  New(Select(Select(Ident(scala), annotation), tailrec)),
                  _
                ),
                List()
              ) =>
            scala.toString == "scala" &&
            annotation.toString() == "annotation" &&
            tailrec.toString() == "tailrec"
          case _ => false
        }
      )
      treeCopy.DefDef(
        defdef,
        copiedMods,
        defdef.name,
        defdef.tparams,
        defdef.vparamss,
        defdef.tpt,
        defdef.rhs
      )
    }

    override def transform(tree: Tree): Tree = tree match {
      case tree: DefDef if tree.pos.line == line =>
        insertAt(tree.pos)(
          filterOutTailRec(
            treeCopy.DefDef(
              tree,
              tree.mods,
              tree.name,
              tree.tparams,
              tree.vparamss,
              tree.tpt,
              mkExprBlock(tree.rhs)
            )
          )
        )
      case tree: DefDef =>
        super.transform(filterOutTailRec(tree))
      case vd: ValDef if vd.pos.line == line =>
        insertAt(vd.pos)(
          treeCopy.ValDef(
            vd,
            vd.mods,
            vd.name,
            vd.tpt,
            mkExprBlock(vd.rhs)
          )
        )
      case tree if tree.pos.line == line =>
        insertAt(tree.pos)(mkExprBlock(tree))
      case tree: PackageDef =>
        val transformed = super.transform(tree).asInstanceOf[PackageDef]
        if (expressionInserted) {
          expressionInserted = false
          atPos(tree.pos)(
            treeCopy.PackageDef(
              transformed,
              transformed.pid,
              transformed.stats ++ parsedExpressionClassAndObject
            )
          )
        } else {
          transformed
        }
      case _ =>
        super.transform(tree)
    }

    private def insertAt(pos: Position)(tree: Tree): Tree = {
      expressionInserted = true
      atPos(pos)(tree)
    }

    private def mkExprBlock(tree: Tree): Tree = {
      val block =
        if (tree.isDef)
          Block(List(parsedExpression, tree), Literal(Constant(())))
        else
          Block(List(parsedExpression), tree)
      addExpressionAttachment(block)
    }

    // `ExpressionAttachment` allows to find the inserted expression later on
    private def addExpressionAttachment(tree: Tree): Tree = {
      val attachments = tree.attachments.update(ExpressionAttachment)
      tree.setAttachments(attachments)
    }
  }
}
