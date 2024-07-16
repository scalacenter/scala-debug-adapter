package ch.epfl.scala.debugadapter.internal.stacktrace

import com.sun.jdi
import com.microsoft.java.debug.core.adapter.stacktrace.DecodedField

final case class JavaField(field: jdi.Field) extends DecodedField {
  def format: String = {
    field.name()
  }
}
