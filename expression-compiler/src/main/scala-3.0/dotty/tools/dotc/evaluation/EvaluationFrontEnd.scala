package dotty.tools.dotc.evaluation

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.EvaluationContext
import dotty.tools.dotc.ast
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.report
import dotty.tools.dotc.typer.FrontEnd
import dotty.tools.dotc.typer.ImportInfo.withRootImports
import dotty.tools.dotc.util.NoSourcePosition
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.util.Stats.record

class EvaluationFrontEnd(using EvaluationContext)(using Context) extends FrontEnd:
  private val insertExpression = InsertExpression()

  private var remaining: List[Context] = Nil
  private var firstXmlPos: SourcePosition = NoSourcePosition

  override def runOn(units: List[CompilationUnit])(using
      Context
  ): List[CompilationUnit] =
    val unitContexts =
      for unit <- units yield
        report.inform(s"compiling ${unit.source}")
        ctx.fresh.setCompilationUnit(unit).withRootImports
    unitContexts.foreach(parse(using _))
    record("parsedTrees", ast.Trees.ntrees)

    unitContexts.foreach(insertExpression.run(using _))

    remaining = unitContexts
    while remaining.nonEmpty do
      enterSyms(using remaining.head)
      remaining = remaining.tail

    if firstXmlPos.exists && !defn.ScalaXmlPackageClass.exists then
      report.error(
        """To support XML literals, your project must depend on scala-xml.
          |See https://github.com/scala/scala-xml for more information.""".stripMargin,
        firstXmlPos
      )

    unitContexts.foreach(typeCheck(using _))
    record("total trees after typer", ast.Trees.ntrees)
    unitContexts.foreach(
      javaCheck(using _)
    ) // after typechecking to avoid cycles

    val newUnits =
      unitContexts.map(_.compilationUnit).filterNot(discardAfterTyper)
    ctx.run.checkSuspendedUnits(newUnits)
    newUnits
end EvaluationFrontEnd
