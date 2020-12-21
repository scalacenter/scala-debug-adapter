package sbtdap

import ch.epfl.scala.bsp

object SbtBspServerContext extends _root_.dap.BspServerContext {

  type Project = sbt.Project
  type State = sbt.State

  override protected def getProjectFromBuildTargetIdentifier(
    target: bsp.BuildTargetIdentifier,
    state: State
  ): Either[String, Option[Project]] = {

    //       val targets = spaceDelimited().parsed.map(uri => BuildTargetIdentifier(URI.create(uri)))
    //      val filter = ScopeFilter.in(targets.map(workspace))

    //ProjectUris.getProjectDagFromUri(projectUri, state)
    ???
  }
}