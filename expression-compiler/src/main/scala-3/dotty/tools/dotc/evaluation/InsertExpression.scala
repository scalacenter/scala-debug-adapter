package dotty.tools.dotc.evaluation

import dotty.tools.dotc.EvaluationContext
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.untpd._
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.util.SourceFile

/**
 * This phase:
 * - inserts the expression that is being evaluated in the line of the breakpoint
 * - inserts `Expression` class in a proper package
 */
class InsertExpression(using
    evalCtx: EvaluationContext
) extends Phase:
  override def phaseName: String = InsertExpression.name
  override def isCheckable: Boolean = false

  private val expressionClassSource =
    s"""class ${evalCtx.expressionClassName}(names: Array[String], values: Array[Object]):
       |  val valuesByName = names.zip(values).toMap
       |
       |  def evaluate(): Unit =
       |    ()
       |
       |  def callPrivate(obj: Any, methodName: String, paramTypeNames: Array[String], args: Array[Object]) =
       |    val expectedParamTypeNames = paramTypeNames.map(paramTypeName => paramTypeName.asInstanceOf[String])
       |    val parameterTypes = args.map(parameterType => parameterType.getClass)
       |    val method = obj
       |      .getClass()
       |      .getDeclaredMethods()
       |      .filter(declaredMethod => declaredMethod.getName() == methodName)
       |      .find(method => {
       |        val paramTypeNames = method
       |          .getParameterTypes()
       |          .map(parameterType => parameterType.getName())
       |        val paramTypeNamesMatch = expectedParamTypeNames
       |          .zip(paramTypeNames)
       |          .forall {
       |            case (expectedParamTypeName, paramTypeName) =>
       |              expectedParamTypeName == paramTypeName
       |          }
       |        method.getParameterTypes().size == paramTypeNames.size && paramTypeNamesMatch
       |      })
       |      .get
       |    method.setAccessible(true)
       |    method.invoke(obj, args: _*)
       |""".stripMargin

  override def run(using Context): Unit =
    val parsedExpression = parseExpression(evalCtx.expression)
    val parsedExpressionClass =
      parseExpressionClass(
        expressionClassSource
      )
    val tree = ctx.compilationUnit.untpdTree
    ctx.compilationUnit.untpdTree =
      TreeInserter(parsedExpression, parsedExpressionClass)
        .transform(tree)

  class TreeInserter(expression: Tree, expressionClass: Seq[Tree])
      extends UntypedTreeMap:
    private var expressionInserted = false

    override def transform(tree: Tree)(using Context): Tree =
      tree match
        case tree: PackageDef =>
          val transformed = super.transform(tree).asInstanceOf[PackageDef]
          if (expressionInserted)
            // set to `false` to prevent inserting `Expression` class in other `PackageDef`s
            expressionInserted = false
            cpy.PackageDef(transformed)(
              transformed.pid,
              transformed.stats ++ expressionClass.map(
                _.withSpan(tree.span)
              )
            )
          else transformed
        case tree: Template if isOnBreakpoint(tree) =>
          expressionInserted = true
          val exprBlock = mkExprBlock(expression, Literal(Constant(())))
          val newTemplate = cpy.Template(tree)(body = tree.body :+ exprBlock)
          super.transform(newTemplate)
        case tree @ DefDef(name, paramss, tpt, _) if isOnBreakpoint(tree) =>
          expressionInserted = true
          cpy.DefDef(tree)(
            name,
            paramss,
            tpt,
            mkExprBlock(expression, tree.rhs)
          )
        case tree @ ValDef(name, tpt, _) if isOnBreakpoint(tree) =>
          expressionInserted = true
          cpy.ValDef(tree)(name, tpt, mkExprBlock(expression, tree.rhs))
        case tree if isOnBreakpoint(tree) =>
          expressionInserted = true
          val expr = mkExprBlock(expression, tree)
            .asInstanceOf[Block]
          expr
        case tree =>
          super.transform(tree)

  private def parseExpression(expression: String)(using Context): Tree =
    val expressionSource =
      s"""object Expression:
         |  { $expression }
         |""".stripMargin
    val parsedExpression = parseSource("<wrapped-expression>", expressionSource)
    parsedExpression
      .asInstanceOf[PackageDef]
      .stats
      .head
      .asInstanceOf[ModuleDef]
      .impl
      .body
      .head

  private def parseExpressionClass(
      expressionClassSource: String
  )(using Context): Seq[Tree] =
    val parsedExpressionClass =
      parseSource("source", expressionClassSource)
        .asInstanceOf[PackageDef]
    parsedExpressionClass.stats

  private def parseSource(name: String, source: String)(using Context): Tree =
    val parser = Parsers.Parser(SourceFile.virtual(name, source))
    parser.parse()

  private def isOnBreakpoint(tree: untpd.Tree)(using Context): Boolean =
    val startLine =
      if tree.span.exists then tree.sourcePos.startLine + 1 else -1
    startLine == evalCtx.breakpointLine

  private def mkExprBlock(expr: Tree, tree: Tree)(using
      Context
  ): Tree =
    val exprTermName = termName(evalCtx.expressionClassName.toLowerCase)
    val ident = Ident(exprTermName)
    val valDef = ValDef(exprTermName, TypeTree(), expr)
    if tree.isDef then Block(List(valDef, ident, tree), Literal(Constant(())))
    else Block(List(valDef, ident), tree)
end InsertExpression

object InsertExpression:
  val name: String = "insert-expression"
end InsertExpression
