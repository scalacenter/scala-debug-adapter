package ch.epfl.scala.debugadapter

import java.nio.file.Path

trait DebuggeeRunner {
  def name: String
  def run(listener: DebuggeeListener): CancelableFuture[Unit]
  def classPathEntries: Seq[ClassPathEntry]
  def classPath: Seq[Path] = classPathEntries.map(_.absolutePath)
  def javaRuntime: Option[ClassPathEntry]
  def allEntries = classPathEntries ++ javaRuntime
}
