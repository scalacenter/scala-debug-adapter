package ch.epfl.scala.debugadapter.internal.jdi

import ch.epfl.scala.debugadapter.internal.binary.*

class JdiReferenceType(obj: Any) extends JdiType(obj, "com.sun.jdi.ReferenceType") with ClassType
