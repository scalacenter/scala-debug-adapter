package ch.epfl.scala.debugadapter

case class ScalaVersion(value: String) extends Ordered[ScalaVersion] {

  def isScala2: Boolean = value.startsWith("2")
  def isScala3: Boolean = value.startsWith("3")
  def isScala212: Boolean = value.startsWith("2.12")
  def isScala213: Boolean = value.startsWith("2.13")
  def isScala33: Boolean = value.startsWith("3.3")
  def isScala34: Boolean = value.startsWith("3.4")
  def isAfter21317: Boolean = parts match {
    case (2, 13, patch) => patch >= 17
    case _ => false
  }

  def isAfter337: Boolean = parts match {
    case (3, 3, patch) => patch >= 7
    case _ => false
  }

  def parts: (Int, Int, Int) = {
    val regex = "(\\d+)\\.(\\d+)\\.(\\d+)(-.+)?".r
    regex
      .unapplySeq(value)
      .collect { case major :: minor :: patch :: tail =>
        (major.toInt, minor.toInt, patch.toInt)
      }
      .get
  }

  def minor: Int = parts match {
    case (_, minor, _) => minor
  }

  override def compare(that: ScalaVersion): Int =
    (parts, that.parts) match {
      case ((x, _, _), (y, _, _)) if x != y => x - y
      case ((_, x, _), (_, y, _)) if x != y => x - y
      case ((_, _, x), (_, _, y)) if x != y => x - y
      case _ => 0
    }

  def binaryVersion: String = if (isScala3) "3" else if (isScala213) "2.13" else "2.12"

  def isRelease: Boolean = !value.contains("-")

  override def toString: String = value
}

object ScalaVersion {
  val `2.11` = ScalaVersion(value = "2.11.12")
  val `2.12` = ScalaVersion(BuildInfo.scala212)
  val `2.13` = ScalaVersion(BuildInfo.scala213)
  val `3.0` = ScalaVersion(BuildInfo.scala30)
  val `3.1+` = ScalaVersion(BuildInfo.scala31Plus)
  val `3.4+` = ScalaVersion(BuildInfo.scala34Plus)
  val `3.7.2+` = ScalaVersion(BuildInfo.scala372Plus)
  val `3.5.0` = ScalaVersion("3.5.0")
}
