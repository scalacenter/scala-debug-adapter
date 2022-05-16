package dotty.tools.dotc

import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core.Symbols.ClassSymbol
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.util.SourceFile

import scala.collection.mutable
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.core.Names
import dotty.tools.dotc.core.Names.TermName

class EvaluationContext(
    val expressionClassName: String,
    val breakpointLine: Int,
    val expression: String,
    val defNames: Set[String]
):
  val expressionTermName: TermName =
    Names.termName(expressionClassName.toLowerCase.toString)

  var expressionOwners: List[Symbol] = _
  var expressionTree: Tree = _
  var expressionSymbol: Symbol = _
  var expressionType: Type = _
  var expressionThis: ClassSymbol = _
  var evaluateMethod: Symbol = _
  var originalThis: ClassSymbol = _
  var defTypes: mutable.Map[String, Type] = mutable.Map()
  var nestedMethods: mutable.Map[SymDenotation, DefDef] = mutable.Map()
