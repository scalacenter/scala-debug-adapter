import ch.epfl.scala.debugadapter.testfmk._

val checkDebugJava = inputKey[Unit]("Check that the -g option has been added")

def checkDebugJavaTask = Def.inputTask {
  val uri = (Compile / startMainClassDebugSession).evaluated
  val source = (Compile / sources).value.head.toPath
  implicit val context: TestingContext = TestingContext(source, "3.3.0")
  DebugTest.check(uri)(Breakpoint(source, 6), Evaluation.success("x", 1))
}
def checkDebugJavaWithGSpecializedTask = Def.inputTask {
  val uri = (Compile / startMainClassDebugSession).evaluated
  val source = (Compile / sources).value.head.toPath
  implicit val context: TestingContext = TestingContext(source, "3.3.0")
  DebugTest.check(uri)(Breakpoint(source, 6), Evaluation.failed("x", "Missing scala-expression-compiler"))
}

lazy val debugJavaWithG =
  project
    .in(file("./withG"))
    .settings(
      javacOptions += "-g",
      checkDebugJava := checkDebugJavaTask.evaluated
    )

lazy val debugJavaWithoutG =
  project
    .in(file("./withoutG"))
    .settings(checkDebugJava := checkDebugJavaTask.evaluated)

lazy val debugJavaWithGSpecialized =
  project
    .in(file("./withGSpecialized"))
    .settings(
      javacOptions += "-g:lines,source",
      checkDebugJava := checkDebugJavaWithGSpecializedTask.evaluated
    )
