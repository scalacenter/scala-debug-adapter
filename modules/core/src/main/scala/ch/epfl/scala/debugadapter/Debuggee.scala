package ch.epfl.scala.debugadapter

import java.nio.file.Path
import java.io.File
import java.io.Closeable

trait Debuggee {
  def name: String
  def scalaVersion: ScalaVersion
  def run(listener: DebuggeeListener): CancelableFuture[Unit]
  def modules: Seq[Module]
  def libraries: Seq[Library]
  def unmanagedEntries: Seq[UnmanagedEntry]
  def javaRuntime: Option[JavaRuntime]
  def observeClassUpdates(onClassUpdate: Seq[String] => Unit): Closeable

  def moduleEntries: Seq[ModuleEntry] = modules
  def managedEntries: Seq[ManagedEntry] = moduleEntries ++ libraries
  def classPathEntries: Seq[ClassPathEntry] = managedEntries ++ unmanagedEntries
  def classPath: Seq[Path] = classPathEntries.flatMap {
    case module: MultiOutputModule => module.fullClassPath
    case entry => Seq(entry.absolutePath)
  }
  def classEntries: Seq[ClassEntry] = classPathEntries ++ javaRuntime
  def classPathString: String = classPath.mkString(File.pathSeparator)
}
