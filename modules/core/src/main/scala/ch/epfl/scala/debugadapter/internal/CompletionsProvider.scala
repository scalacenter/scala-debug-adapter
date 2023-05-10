package ch.epfl.scala.debugadapter.internal

import com.microsoft.java.debug.core.adapter.ICompletionsProvider
import com.microsoft.java.debug.core.protocol.Types
import com.sun.jdi.StackFrame

import java.util

object CompletionsProvider extends ICompletionsProvider {
  override def codeComplete(
      frame: StackFrame,
      snippet: String,
      line: Int,
      column: Int
  ): util.List[Types.CompletionItem] = util.Collections.emptyList()
}
