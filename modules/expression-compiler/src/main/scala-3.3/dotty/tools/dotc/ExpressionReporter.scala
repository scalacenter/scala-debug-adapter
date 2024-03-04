package dotty.tools.dotc

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.reporting.AbstractReporter
import dotty.tools.dotc.reporting.Diagnostic
import scala.util.matching.Regex

class ExpressionReporter(reportError: String => Unit) extends AbstractReporter:
  override def doReport(dia: Diagnostic)(using Context): Unit =
    // When printing trees, we trim what comes after 'def evaluate'
    // val message = messageAndPos(dia)
    // val shortMessage = message.split(Regex.quote("\u001b[33mdef\u001b[0m \u001b[36mgetLocalValue\u001b[0m"))(0)
    // println(shortMessage)
    dia match
      case error: Diagnostic.Error =>
        val newPos = error.pos.source.positionInUltimateSource(error.pos)
        val errorWithNewPos = new Diagnostic.Error(error.msg, newPos)
        reportError(stripColor(messageAndPos(errorWithNewPos)))
      case _ =>
        // TODO report the warnings
        ()
