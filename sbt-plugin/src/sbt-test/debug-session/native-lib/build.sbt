import ch.epfl.scala.debugadapter.testfmk._

val checkDebugSession = inputKey[Unit]("Check the test suite debug session")

libraryDependencies += "org.lmdbjava" % "lmdbjava" % "0.8.2"
scalaVersion := "2.13.7"

checkDebugSession := {
  val uri = (Compile / startMainClassDebugSession).evaluated
  DebugTest.check(uri)(Outputed("Success"))
}
