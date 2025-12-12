import ch.epfl.scala.debugadapter.testfmk._
import ch.epfl.scala.debugadapter.DebugConfig

val scalaCollectionCompat = inputKey[Unit]("Check the presence of scala-collection-compat")
val scalaV = "2.12.21"

def checkScalaCollectionCompat = Def.inputTask {
  val uri = (Compile / startMainClassDebugSession).evaluated
  val source = (Compile / sources).value.head.toPath
  implicit val context: TestingContext = TestingContext(source, scalaV)
  DebugTest.check(uri)(Breakpoint(source, 6), Evaluation.success("x", 1))
}

lazy val scala212Compat =
  project
    .in(file("."))
    .settings(
      scalaVersion := scalaV,
      scalaCollectionCompat := checkScalaCollectionCompat.evaluated,
      debugAdapterConfig := DebugConfig.default.copy(evaluationMode = DebugConfig.ScalaEvaluationOnly)
    )
