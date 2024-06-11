package ch.epfl.scala.debugadapter.internal.jdi

import ch.epfl.scala.decoder.binary
import java.util as ju
import scala.jdk.CollectionConverters.*

class JdiClassLoader(obj: Any)
    extends binary.BinaryClassLoader
    with JavaReflection(obj, "com.sun.jdi.ClassLoaderReference"):
  override def loadClass(name: String): binary.ClassType =
    visibleClasses.find(_.name == name).get

  private def visibleClasses: Seq[JdiReferenceType] =
    invokeMethod[ju.List[Any]]("visibleClasses").asScala.map(JdiReferenceType.apply(_)).toSeq
