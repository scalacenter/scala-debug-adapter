import ch.epfl.scala.debugadapter.testfmk._

val checkDebugSession = inputKey[Unit]("Check the attach debug session")

scalaVersion := "2.12.14"
fork := true
javaOptions += "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=1045"
checkDebugSession := {
  val uri = (Compile / startRemoteDebugSession).evaluated
  val source = (Compile / sources).value.head.toPath
  (Compile / bgRun).toTask("").value

  DebugTest.check(uri, attach = Some(1045))(Breakpoint(source, 5))
}
