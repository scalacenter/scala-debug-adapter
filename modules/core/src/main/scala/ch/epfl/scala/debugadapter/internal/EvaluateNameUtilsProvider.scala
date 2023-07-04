package ch.epfl.scala.debugadapter.internal

import com.microsoft.java.debug.core.adapter.variables.IVariableProvider

private[internal] object VariableProvider extends IVariableProvider {
  override def getEvaluateName(name: String, containerName: String, isArrayElement: Boolean): String =
    (name, containerName, isArrayElement) match {
      case (null, _, _) => null
      case (_, null, true) => null
      case (name, containerName, true) => s"$containerName($name)"
      case (name, null, _) => name
      case _ => s"$containerName.$name"
    }
}
