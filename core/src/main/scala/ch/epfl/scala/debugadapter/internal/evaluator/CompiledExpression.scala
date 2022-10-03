package ch.epfl.scala.debugadapter.internal.evaluator

import java.nio.file.Path

final case class CompiledExpression(classDir: Path, className: String)
