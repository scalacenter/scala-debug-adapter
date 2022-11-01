package ch.epfl.scala.debugadapter

import scala.util.Try

trait DebugToolsResolver {
  def resolveExpressionCompiler(scalaVersion: ScalaVersion): Try[ClassLoader]
  def resolveStepFilter(scalaVersion: ScalaVersion): Try[ClassLoader]
}
