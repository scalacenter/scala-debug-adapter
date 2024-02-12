package ch.epfl.scala.debugadapter.internal.stacktrace

import tastyquery.Names.*

object CommonNames:
  val anonClass: SimpleTypeName = typeName("$anon")

  val anonFun: SimpleName = termName("$anonfun")

  val Predef: SimpleName = termName("Predef")
