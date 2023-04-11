package ch.epfl.scala.debugadapter.internal.evaluator

import java.nio.file.Path
import scala.meta.Stat

sealed trait PreparedExpression
final case class CompiledExpression(classDir: Path, className: String) extends PreparedExpression
final case class ParsedExpression(expression: Stat) extends PreparedExpression
final case class LocalValue(name: String) extends PreparedExpression
final case class PlainLogMessage(message: String) extends PreparedExpression
