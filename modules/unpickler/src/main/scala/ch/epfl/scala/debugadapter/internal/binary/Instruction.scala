package ch.epfl.scala.debugadapter.internal.binary

enum Instruction:
  case Method(opcode: Int, owner: String, name: String, descriptor: String, isInterface: Boolean)
  case Field(opcode: Int, owner: String, name: String, descriptor: String)
