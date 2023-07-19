package ch.epfl.scala.debugadapter.internal.javareflect

import ch.epfl.scala.debugadapter.internal.binary

class JavaReflectReferencType(cls: Class[?]) extends JavaReflectType(cls) with binary.ReferenceType
