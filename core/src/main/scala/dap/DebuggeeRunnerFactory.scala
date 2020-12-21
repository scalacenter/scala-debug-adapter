package dap

import ch.epfl.scala.bsp

trait DebuggeeRunnerFactory[Project, State] {

  protected def makeMainClassDebugAdapter(
    project: Project,
    state: State,
    mainClass: bsp.ScalaMainClass
  ): MainClassDebugAdapter

  protected def makeTestSuiteDebugAdapter(
    projects: Seq[Project],
    state: State,
    filters: List[String],
  ): TestSuiteDebugAdapter

  protected def makeAttachRemoteDebugAdapter(state: State): DebuggeeRunner

  final def forMainClass(
    projects: Seq[Project],
    state: State,
    mainClass: bsp.ScalaMainClass,
  ): Either[String, DebuggeeRunner] = {
    projects match {
      case Seq() => Left(s"No projects specified for main class: [$mainClass]")
      case Seq(project) => Right(makeMainClassDebugAdapter(project, state, mainClass))
      case projects => Left(s"Multiple projects specified for main class [$mainClass]: $projects")
    }
  }

  final def forTestSuite(
    projects: Seq[Project],
    state: State,
    filters: List[String],
  ): Either[String, DebuggeeRunner] = {
    projects match {
      case Seq() => Left(s"No projects specified for the test suites: [${filters.sorted}]")
      case projects => Right(makeTestSuiteDebugAdapter(projects, state, filters))
    }
  }

  final def forAttachRemote(state: State): Either[String, DebuggeeRunner] =
    Right(makeAttachRemoteDebugAdapter(state))
}