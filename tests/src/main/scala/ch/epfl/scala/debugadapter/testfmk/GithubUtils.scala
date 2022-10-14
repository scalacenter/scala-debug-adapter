package ch.epfl.scala.debugadapter

object GithubUtils {
  def isCI(): Boolean = System.getenv("CI") != null
}
