package ch.epfl.scala.debugadapter

case class ScalaVersion(value: String) {
  def isScala2: Boolean = value.startsWith("2")
  def isScala3: Boolean = value.startsWith("3")
  def isScala212: Boolean = value.startsWith("2.12")
  def isScala213: Boolean = value.startsWith("2.13")
  def isScala30: Boolean = value.startsWith("3.0")
  def isScala32: Boolean = value.startsWith("3.2")

  def binaryVersion: String = if (isScala3) "3" else if (isScala213) "2.13" else "2.12"

  override def toString: String = value
}

object ScalaVersion {
  val `2.12` = ScalaVersion("2.12.17")
  val `2.13` = ScalaVersion(BuildInfo.defaultScala2Version)
  val `3.0` = ScalaVersion("3.0.2")
  val `3.1` = ScalaVersion("3.1.2")
  val `3.2` = ScalaVersion(BuildInfo.defaultScala3Version)
}
