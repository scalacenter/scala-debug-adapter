package ch.epfl.scala.debugadapter.testfmk

import com.sun.jdi.*

import java.{util => ju}

final case class FakeJdiType(
    override val name: String
) extends ReferenceType {

  override def virtualMachine(): VirtualMachine = ???

  override def signature(): String = ???

  override def compareTo(o: ReferenceType): Int = ???

  override def modifiers(): Int = ???

  override def isPrivate(): Boolean = ???

  override def isPackagePrivate(): Boolean = ???

  override def isProtected(): Boolean = ???

  override def isPublic(): Boolean = ???

  override def genericSignature(): String = ???

  override def classLoader(): ClassLoaderReference = ???

  override def sourceName(): String = ???

  override def sourceNames(stratum: String): ju.List[String] = ???

  override def sourcePaths(stratum: String): ju.List[String] = ???

  override def sourceDebugExtension(): String = ???

  override def isStatic(): Boolean = ???

  override def isAbstract(): Boolean = ???

  override def isFinal(): Boolean = ???

  override def isPrepared(): Boolean = ???

  override def isVerified(): Boolean = ???

  override def isInitialized(): Boolean = ???

  override def failedToInitialize(): Boolean = ???

  override def fields(): ju.List[Field] = ???

  override def visibleFields(): ju.List[Field] = ???

  override def allFields(): ju.List[Field] = ???

  override def fieldByName(fieldName: String): Field = ???

  override def methods(): ju.List[Method] = ???

  override def visibleMethods(): ju.List[Method] = ???

  override def allMethods(): ju.List[Method] = ???

  override def methodsByName(name: String): ju.List[Method] = ???

  override def methodsByName(name: String, signature: String): ju.List[Method] = ???

  override def nestedTypes(): ju.List[ReferenceType] = ???

  override def getValue(field: Field): Value = ???

  override def getValues(fields: ju.List[_ <: Field]): ju.Map[Field, Value] = ???

  override def classObject(): ClassObjectReference = ???

  override def allLineLocations(): ju.List[Location] = ???

  override def allLineLocations(stratum: String, sourceName: String): ju.List[Location] = ???

  override def locationsOfLine(lineNumber: Int): ju.List[Location] = ???

  override def locationsOfLine(stratum: String, sourceName: String, lineNumber: Int): ju.List[Location] = ???

  override def availableStrata(): ju.List[String] = ???

  override def defaultStratum(): String = ???

  override def instances(maxInstances: Long): ju.List[ObjectReference] = ???

  override def majorVersion(): Int = ???

  override def minorVersion(): Int = ???

  override def constantPoolCount(): Int = ???

  override def constantPool(): Array[Byte] = ???
}
