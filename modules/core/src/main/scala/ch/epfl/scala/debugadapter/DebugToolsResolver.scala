package ch.epfl.scala.debugadapter

import scala.util.Try

trait DebugToolsResolver {
  def resolveExpressionCompiler(scalaVersion: ScalaVersion): Try[ClassLoader]
  def resolveUnpickler(scalaVersion: ScalaVersion): Try[ClassLoader]
}
