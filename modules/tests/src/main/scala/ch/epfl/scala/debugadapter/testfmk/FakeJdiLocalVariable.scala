package ch.epfl.scala.debugadapter.testfmk

import com.sun.jdi.*

final case class FakeJdiLocalVariable(
    override val name: String,
    override val `type`: Type
) extends LocalVariable {

  override def virtualMachine(): VirtualMachine = ???

  override def compareTo(o: LocalVariable): Int = ???

  override def typeName(): String = ???

  override def signature(): String = ???

  override def genericSignature(): String = ???

  override def isVisible(frame: StackFrame): Boolean = ???

  override def isArgument(): Boolean = ???
}
