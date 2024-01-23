package dotty.tools.dotc.evaluation

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.Symbol

object SymUtils:
  export dotty.tools.dotc.transform.SymUtils.{isLocal => _, enclosingMethodOrClass => _, *}

  extension (self: Symbol) def isLocal(using Context) = dotty.tools.dotc.transform.SymUtils.isLocal(self)
