package ch.epfl.debugadapter.jdi

class Method(obj: Any):
  def name: String =
    val method = obj.getClass.getMethod("name")
    method.invoke(obj).asInstanceOf[String]
