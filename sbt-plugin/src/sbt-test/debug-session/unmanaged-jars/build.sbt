import scala.util.Properties
import ch.epfl.scala.debugadapter.testfmk._
import java.nio.file.Paths

val checkDebugSession = inputKey[Unit]("Check the main class debug session")

val jdkHome = Paths.get(Properties.jdkHome)

scalaVersion := "2.12.14"
javaHome := Some(jdkHome.toFile)
checkDebugSession := {
  val uri = (Compile / startMainClassDebugSession).evaluated
  DebugTest.check(uri)(Outputed("com.sun.jdi.Value"))
}
