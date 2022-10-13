import scala.util.Properties
import ch.epfl.scala.debugadapter.testfmk._
import java.nio.file.Paths

val checkDebugSession = inputKey[Unit]("Check the main class debug session")

val jdkHome = Paths.get(Properties.jdkHome)
val jdkSourceZip = jdkHome.resolve("src.zip")

scalaVersion := "2.12.14"
javaHome := Some(jdkHome.toFile)
checkDebugSession := {
  val uri = (Compile / startMainClassDebugSession).evaluated
  val source = (Compile / sources).value.head.toPath

  DebugTest.check(uri)(
    Breakpoint(source, 5),
    StepIn.method("PrintStream.println(String)")
  )
}
