package ch.epfl.scala.debugadapter.internal

object ByteCode {
  // return void from method
  val RETURN: Byte = 0xb1.toByte
  val NEW: Byte = 0xbb.toByte
  val ANEWARRAY: Byte = 0xbd.toByte
  val MULTIANEWARRAY: Byte = 0xc5.toByte
  val LDC: Byte = 0x12.toByte
  val INSTANCEOF: Byte = 0xc1.toByte
  val CHECKCAST: Byte = 0xc0.toByte
}
