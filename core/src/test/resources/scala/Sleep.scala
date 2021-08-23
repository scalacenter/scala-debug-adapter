package scaladebug.test

object Sleep {
  def main(args: Array[String]): Unit = {
    synchronized(wait()) // block for all eternity
  }
}
