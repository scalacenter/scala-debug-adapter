package ch.epfl.scala.debugadapter.testfmk

import com.microsoft.java.debug.core.protocol.Types.StackFrame

object DebugState {
  var state = DebugCheckState(null, -1, null, false)
}
