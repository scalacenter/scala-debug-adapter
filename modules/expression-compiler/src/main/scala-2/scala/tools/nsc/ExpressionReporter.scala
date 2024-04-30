package scala.tools.nsc

import scala.tools.nsc.reporters.FilteringReporter
import scala.reflect.internal.util.Position
import scala.reflect.internal.Reporter
import scala.annotation.nowarn

class ExpressionReporter(reportError: String => Unit, val settings: Settings) extends FilteringReporter {

  @nowarn
  override def doReport(pos: Position, msg: String, severity: Severity): Unit = {
    severity match {
      case Reporter.ERROR =>
        val newPos = pos.source.positionInUltimateSource(pos)
        val formatted = Position.formatMessage(newPos, s"${clabel(severity)}$msg", shortenFile = false)
        reportError(formatted)
      case _ =>
        // TODO report the warnings
        ()
    }
  }

  private def clabel(severity: Severity): String = severity match {
    case Reporter.ERROR => "error: "
    case Reporter.WARNING => "warning: "
    case _ => ""
  }
}
