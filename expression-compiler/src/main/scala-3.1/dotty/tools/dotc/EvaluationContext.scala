package dotty.tools.dotc

import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core.Symbols.ClassSymbol
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.util.SourceFile

import scala.collection.mutable
import dotty.tools.dotc.core.SymDenotations.SymDenotation

class EvaluationContext(
    val sourceFile: SourceFile,
    val expressionClassName: String,
    val breakpointLine: Int,
    val expression: String,
    val defNames: Set[String]
):
  val expressionIdentName: String = expressionClassName.toLowerCase

  var expressionOwners: List[Symbol] = _
  var expressionValDef: ValDef = _
  var expressionIdent: Ident = _
  var expressionType: Type = _
  var expressionThis: ClassSymbol = _
  var evaluateMethod: Symbol = _
  var originalThis: ClassSymbol = _
  var defTypes: mutable.Map[String, Type] = mutable.Map()
  var nestedMethods: mutable.Map[SymDenotation, DefDef] = mutable.Map()
end EvaluationContext
