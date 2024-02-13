package ch.epfl.scala.debugadapter.internal.stacktrace

import tastyquery.Classpaths.*

/**
 * Override the TASTy version of all TASTy files to prevent TASTy Query from checking it
 * Better fail at reading a TASTy file that not trying
 */
object CustomClasspath:

  def apply(classpath: Classpath): Classpath =
    classpath.map(CustomClasspathEntry(_))

  private class CustomClasspathEntry(entry: ClasspathEntry) extends ClasspathEntry:
    override def listAllPackages(): List[PackageData] =
      entry.listAllPackages().map(CustomPackageData(_))

  private class CustomPackageData(data: PackageData) extends PackageData:
    override val dotSeparatedName: String = data.dotSeparatedName
    override def listAllClassDatas(): List[ClassData] =
      data.listAllClassDatas().map(CustomClassData(_))
    override def getClassDataByBinaryName(binaryName: String): Option[ClassData] =
      data.getClassDataByBinaryName(binaryName)

  private class CustomClassData(data: ClassData) extends ClassData:
    override def hasClassFile: Boolean = data.hasClassFile
    override val binaryName: String = data.binaryName
    override def readTastyFileBytes(): IArray[Byte] =
      val bytes = data.readTastyFileBytes().unsafeArray
      bytes(4) = (28 | 0x80).toByte // major version
      bytes(5) = (3 | 0x80).toByte // minor version
      bytes(6) = (0 | 0x80).toByte // experimental
      bytes.asInstanceOf[IArray[Byte]]

    override def hasTastyFile: Boolean = data.hasTastyFile
    override def readClassFileBytes(): IArray[Byte] = data.readClassFileBytes()
    override def toString: String = data.toString
