package dotty.tools.dotc

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import scala.collection.mutable
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Contexts.*

class EvaluationContext(
    val expressionClassName: String,
    val breakpointLine: Int,
    val expression: String,
    val defNames: Set[String]
):
  val expressionTermName: TermName =
    termName(expressionClassName.toLowerCase.toString)
  val evaluateName = termName("evaluate")

  var expressionOwners: List[Symbol] = _
  var expressionTree: Tree = _
  var expressionSymbol: Symbol = _
  var expressionType: Type = _
  var expressionClass: ClassSymbol = _
  var originalThis: ClassSymbol = _
  val nestedMethods: mutable.Map[SymDenotation, DefDef] = mutable.Map()

  def evaluateMethod(using Context): Symbol =
    expressionClass.info.decl(evaluateName).symbol
