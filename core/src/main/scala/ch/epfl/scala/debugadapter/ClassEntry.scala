package ch.epfl.scala.debugadapter

trait ClassEntry {
  def name: String
  def sourceEntries: Seq[SourceEntry]
  def classSystems: Seq[ClassSystem]
}
