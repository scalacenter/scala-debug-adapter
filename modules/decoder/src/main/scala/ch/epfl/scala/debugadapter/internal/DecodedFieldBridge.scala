package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.decoder.DecodedField
import ch.epfl.scala.decoder.StackTraceFormatter
import tastyquery.Symbols.*
import tastyquery.Modifiers.*
import ch.epfl.scala.decoder.DecodedField.ValDef
import ch.epfl.scala.decoder.DecodedField.ModuleVal
import ch.epfl.scala.decoder.DecodedField.LazyValOffset
import ch.epfl.scala.decoder.DecodedField.Outer
import ch.epfl.scala.decoder.DecodedField.Capture
import ch.epfl.scala.decoder.DecodedField.LazyValBitmap

class DecodedFieldBridge(field: DecodedField):
  def format: String = field match
    case field: ValDef => field.symbol.name.toString()
    case field: ModuleVal => ""
    case field: LazyValOffset => ""
    case field: Outer => "<outer>"
    case field: ch.epfl.scala.decoder.DecodedField.SerialVersionUID => ""
    case field: Capture => field.symbol.name.toString()
    case field: LazyValBitmap => ""

  def show: Boolean = true

  // formatter.format(variable)
