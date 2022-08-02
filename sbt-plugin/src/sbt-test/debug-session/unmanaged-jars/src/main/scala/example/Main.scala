package example

// this import comes from an unmnaged jar
import com.sun.jdi.Value

object Main {
  def main(args: Array[String]): Unit = {
    val someJdiClass = classOf[Value]
    System.out.println(someJdiClass.getName)
  }
}
