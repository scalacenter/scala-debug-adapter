package ch.epfl.scala.debugadapter.testfmk

object DebugState {
  var clientState: (TestingDebugClient, DebugCheckState) = null

  def state: DebugCheckState = clientState._2
  def client: TestingDebugClient = clientState._1
}
