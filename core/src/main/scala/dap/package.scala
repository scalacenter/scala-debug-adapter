import java.util.function.Supplier
import scala.meta.jsonrpc.{Response => JsonRpcResponse}

package object dap {
  type ProtocolError = JsonRpcResponse.Error

  implicit def toSupplier[A](a: => A): Supplier[A] = new Supplier[A] {
    override def get: A = a
  }
}
