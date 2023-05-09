import ch.epfl.scala.debugadapter.testfmk._

scalaVersion := "2.12.14"
InputKey[Unit]("checkDebugSession") := {
  val uri = (Compile / startMainClassDebugSession).evaluated
  val source = (Compile / sources).value.head.toPath
  DebugTest.check(uri)(Breakpoint(source, 5), Outputed("Hello, World!"))
}

InputKey[Unit]("checkDebugSessionWithSkipConfig") := {
  val uri = (Compile / startMainClassDebugSession).evaluated
  val source = (Compile / sources).value.head.toPath
  DebugTest.check(uri)(Outputed("Hello, World!"))
}
