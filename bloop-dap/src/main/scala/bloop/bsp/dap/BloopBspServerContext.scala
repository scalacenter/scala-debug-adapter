package bloop.bsp.dap

import bloop.bsp.ProjectUris
import bloop.logging.Logger
import ch.epfl.scala.bsp

object BloopBspServerContext extends _root_.dap.BspServerContext {
  type Project = bloop.data.Project
  type State = bloop.engine.State

  def state: State = ???
  def logger: Logger = ???

  def buildToolName: String = "bloop"

  override protected def getProjectFromBuildTargetIdentifier(
    target: bsp.BuildTargetIdentifier,
    state: State
  ): Either[String, Option[Project]] = {
    val projectUri = target.uri.value
    ProjectUris.getProjectDagFromUri(projectUri, state)
  }


}