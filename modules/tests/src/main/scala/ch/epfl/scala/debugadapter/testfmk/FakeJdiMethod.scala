package ch.epfl.scala.debugadapter.testfmk

import com.sun.jdi.*

import java.{util => ju}
import scala.jdk.CollectionConverters.*

final case class FakeJdiMethod(
    override val declaringType: ReferenceType,
    override val name: String,
    override val arguments: ju.List[LocalVariable],
    override val returnType: Type
) extends Method {

  override def signature(): String = ???

  override def virtualMachine(): VirtualMachine = ???

  override def modifiers(): Int = ???

  override def isPrivate(): Boolean = ???

  override def isPackagePrivate(): Boolean = ???

  override def isProtected(): Boolean = ???

  override def isPublic(): Boolean = ???

  override def genericSignature(): String = ???

  override def isStatic(): Boolean = ???

  override def isFinal(): Boolean = ???

  override def isSynthetic(): Boolean = ???

  override def compareTo(o: Method): Int = ???

  override def returnTypeName(): String = ???

  override def argumentTypeNames(): ju.List[String] = ???

  override def argumentTypes(): ju.List[Type] = ???

  override def isAbstract(): Boolean = ???

  override def isSynchronized(): Boolean = ???

  override def isNative(): Boolean = ???

  override def isVarArgs(): Boolean = ???

  override def isBridge(): Boolean = ???

  override def isConstructor(): Boolean = ???

  override def isStaticInitializer(): Boolean = ???

  override def isObsolete(): Boolean = ???

  override def allLineLocations(): ju.List[Location] = ???

  override def allLineLocations(stratum: String, sourceName: String): ju.List[Location] = ???

  override def locationsOfLine(lineNumber: Int): ju.List[Location] = ???

  override def locationsOfLine(stratum: String, sourceName: String, lineNumber: Int): ju.List[Location] = ???

  override def locationOfCodeIndex(codeIndex: Long): Location = ???

  override def variables(): ju.List[LocalVariable] = ???

  override def variablesByName(name: String): ju.List[LocalVariable] = ???

  override def bytecodes(): Array[Byte] = ???

  override def location(): Location = ???
}

object FakeJdiMethod {
  def apply(declaringType: String, name: String)(arguments: (String, String)*)(
      returnType: String = "void"
  ): FakeJdiMethod =
    FakeJdiMethod(
      FakeJdiType(declaringType),
      name,
      arguments.map { case (name, typ) => FakeJdiLocalVariable(name, FakeJdiType(typ)): LocalVariable }.toList.asJava,
      FakeJdiType(returnType)
    )
}
