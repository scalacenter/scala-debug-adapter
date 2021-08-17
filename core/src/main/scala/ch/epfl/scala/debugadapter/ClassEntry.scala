package ch.epfl.scala.debugadapter

trait ClassEntry {
  def sourceEntries: Seq[SourceEntry]
  def classSystems: Seq[ClassSystem]
}
