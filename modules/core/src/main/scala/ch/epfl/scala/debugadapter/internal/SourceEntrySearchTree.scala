package ch.epfl.scala.debugadapter.internal

import scala.collection.mutable.Map

trait ClassSearch {
  def insert(fqcn: String): Unit
  def find(className: String): List[String]
}

class SourceEntrySearchMap extends ClassSearch {
  private val classes = Map[String, List[String]]().withDefaultValue(List.empty)

  private def getLastInnerType(className: String): Option[String] = {
    val pattern = """(.+\$)(.+)$""".r
    className match {
      case pattern(_, innerType) => Some(innerType)
      case _ => None
    }
  }

  def insert(fqcn: String) = {
    val classWithOuters = fqcn.split('.').last
    var className = getLastInnerType(classWithOuters).getOrElse(classWithOuters)

    classes.update(className, fqcn :: classes(className))
  }

  def find(className: String): List[String] = classes(className)
}
