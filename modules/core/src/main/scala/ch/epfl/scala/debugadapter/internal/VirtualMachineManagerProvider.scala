package ch.epfl.scala.debugadapter.internal

import com.microsoft.java.debug.core.adapter.IVirtualMachineManagerProvider
import com.sun.jdi.Bootstrap
import com.sun.jdi.VirtualMachineManager

object VirtualMachineManagerProvider extends IVirtualMachineManagerProvider {
  def getVirtualMachineManager: VirtualMachineManager =
    Bootstrap.virtualMachineManager
}
