package dotty.tools.dotc

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.evaluation.*
import dotty.tools.dotc.transform.ElimByName
import dotty.tools.dotc.util.SourceFile

class ExpressionCompiler(using ExpressionContext)(using Context) extends Compiler:

  override protected def frontendPhases: List[List[Phase]] =
    List(EvaluationFrontEnd()) :: super.frontendPhases.tail

  override protected def transformPhases: List[List[Phase]] =
    // the ExtractExpression phase should be after ElimByName and ExtensionMethods,
    // and before LambdaLift
    val transformPhases = super.transformPhases
    val index = transformPhases.indexWhere(_.exists(_.phaseName == ElimByName.name))
    val (before, after) = transformPhases.splitAt(index + 1)
    (before :+ List(ExtractExpression())) ++ (after :+ List(ResolveReflectEval()))
