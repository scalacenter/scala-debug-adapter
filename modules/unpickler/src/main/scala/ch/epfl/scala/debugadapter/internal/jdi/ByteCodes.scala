package ch.epfl.scala.debugadapter.internal.jdi

import java.nio.charset.StandardCharsets
import scala.collection.mutable.Buffer
import ch.epfl.scala.debugadapter.internal.binary.Instruction
import ch.epfl.scala.debugadapter.internal.jdi.ConstantPool.INTERFACE_METHODREF_TAG

private class ByteReader(bytes: Array[Byte]):
  def unsignedShort(offset: Int): Int =
    (unsignedByte(offset) << 8) | unsignedByte(offset + 1)

  def byte(offset: Int): Byte = bytes(offset)

  def unsignedByte(offset: Int): Int = bytes(offset) & 0xff

  def int(offset: Int): Int =
    (unsignedByte(offset) << 24) | (unsignedByte(offset) << 16) | (unsignedByte(offset) << 8) | unsignedByte(offset)

  def unexpected(offset: Int): Nothing =
    throw new IllegalArgumentException(s"Unexpected byte ${bytes(offset)} at position $offset")

  def utf8(start: Int, size: Int): String =
    val sub = bytes.slice(start, start + size)
    new String(sub, StandardCharsets.UTF_8)

private class ConstantPool(offsets: IndexedSeq[Int], reader: ByteReader):
  def readUtf8(index: Int): String =
    val offset = offsets(index - 1)
    val size = reader.unsignedShort(offset)
    reader.utf8(offset + 2, size)

  def readClass(index: Int): String =
    val offset = offsets(index - 1)
    readUtf8(reader.unsignedShort(offset)).replace('/', '.')

  def readNameAndType(index: Int): (String, String) =
    val offset = offsets(index - 1)
    (readUtf8(reader.unsignedShort(offset)), readUtf8(reader.unsignedShort(offset + 2)))

  def readField(opcode: Int, index: Int): Instruction.Field =
    val offset = offsets(index - 1)
    val owner = readClass(reader.unsignedShort(offset))
    val (name, descriptor) = readNameAndType(reader.unsignedShort(offset + 2))
    Instruction.Field(opcode, owner, name, descriptor)

  def readMethod(opcode: Int, index: Int): Instruction.Method =
    val offset = offsets(index - 1)
    val owner = readClass(reader.unsignedShort(offset))
    val (name, descriptor) = readNameAndType(reader.unsignedShort(offset + 2))
    val isInterface = reader.byte(offset - 1) == INTERFACE_METHODREF_TAG
    Instruction.Method(opcode, owner, name, descriptor, isInterface)

private object ConstantPool:
  val CLASS_TAG: Byte = 7
  val FIELDREF_TAG: Byte = 9
  val METHODREF_TAG: Byte = 10
  val INTERFACE_METHODREF_TAG: Byte = 11
  val STRING_TAG: Byte = 8
  val INTEGER_TAG: Byte = 3
  val FLOAT_TAG: Byte = 4
  val LONG_TAG: Byte = 5
  val DOUBLE_TAG: Byte = 6
  val NAME_AND_TYPE_TAG: Byte = 12
  val UTF8_TAG: Byte = 1
  val METHOD_HANDLE_TAG: Byte = 15
  val METHOD_TYPE_TAG: Byte = 16
  val DYNAMIC_TAG: Byte = 17
  val INVOKE_DYNAMIC_TAG: Byte = 18
  val MODULE_TAG: Byte = 19
  val PACKAGE_TAG: Byte = 20

  def apply(bytes: Array[Byte]): ConstantPool =
    val offsets = Buffer.empty[Int]
    val reader = new ByteReader(bytes)
    var current = 0
    while current < bytes.size do
      offsets += current + 1
      reader.byte(current) match
        case FIELDREF_TAG | METHODREF_TAG | INTERFACE_METHODREF_TAG | INTEGER_TAG | FLOAT_TAG | NAME_AND_TYPE_TAG |
            DYNAMIC_TAG | INVOKE_DYNAMIC_TAG =>
          current += 5
        case LONG_TAG | DOUBLE_TAG =>
          offsets += -1
          current += 9
        case UTF8_TAG =>
          val size = reader.unsignedShort(current + 1)
          current += 3 + size
        case METHOD_HANDLE_TAG => current += 4
        case CLASS_TAG | STRING_TAG | METHOD_TYPE_TAG | PACKAGE_TAG | MODULE_TAG =>
          current += 3
        case tag => reader.unexpected(current)
    new ConstantPool(offsets.toIndexedSeq, reader)

object ByteCodes:
  val NOP: Int = 0
  val ACONST_NULL: Int = 1
  val ICONST_M1: Int = 2
  val ICONST_0: Int = 3
  val ICONST_1: Int = 4
  val ICONST_2: Int = 5
  val ICONST_3: Int = 6
  val ICONST_4: Int = 7
  val ICONST_5: Int = 8
  val LCONST_0: Int = 9
  val LCONST_1: Int = 10
  val FCONST_0: Int = 11
  val FCONST_1: Int = 12
  val FCONST_2: Int = 13
  val DCONST_0: Int = 14
  val DCONST_1: Int = 15
  val BIPUSH: Int = 16
  val SIPUSH: Int = 17
  val LDC: Int = 18
  val ILOAD: Int = 21
  val LLOAD: Int = 22
  val FLOAD: Int = 23
  val DLOAD: Int = 24
  val ALOAD: Int = 25
  val IALOAD: Int = 46
  val LALOAD: Int = 47
  val FALOAD: Int = 48
  val DALOAD: Int = 49
  val AALOAD: Int = 50
  val BALOAD: Int = 51
  val CALOAD: Int = 52
  val SALOAD: Int = 53
  val ISTORE: Int = 54
  val LSTORE: Int = 55
  val FSTORE: Int = 56
  val DSTORE: Int = 57
  val ASTORE: Int = 58
  val IASTORE: Int = 79
  val LASTORE: Int = 80
  val FASTORE: Int = 81
  val DASTORE: Int = 82
  val AASTORE: Int = 83
  val BASTORE: Int = 84
  val CASTORE: Int = 85
  val SASTORE: Int = 86
  val POP: Int = 87
  val POP2: Int = 88
  val DUP: Int = 89
  val DUP_X1: Int = 90
  val DUP_X2: Int = 91
  val DUP2: Int = 92
  val DUP2_X1: Int = 93
  val DUP2_X2: Int = 94
  val SWAP: Int = 95
  val IADD: Int = 96
  val LADD: Int = 97
  val FADD: Int = 98
  val DADD: Int = 99
  val ISUB: Int = 100
  val LSUB: Int = 101
  val FSUB: Int = 102
  val DSUB: Int = 103
  val IMUL: Int = 104
  val LMUL: Int = 105
  val FMUL: Int = 106
  val DMUL: Int = 107
  val IDIV: Int = 108
  val LDIV: Int = 109
  val FDIV: Int = 110
  val DDIV: Int = 111
  val IREM: Int = 112
  val LREM: Int = 113
  val FREM: Int = 114
  val DREM: Int = 115
  val INEG: Int = 116
  val LNEG: Int = 117
  val FNEG: Int = 118
  val DNEG: Int = 119
  val ISHL: Int = 120
  val LSHL: Int = 121
  val ISHR: Int = 122
  val LSHR: Int = 123
  val IUSHR: Int = 124
  val LUSHR: Int = 125
  val IAND: Int = 126
  val LAND: Int = 127
  val IOR: Int = 128
  val LOR: Int = 129
  val IXOR: Int = 130
  val LXOR: Int = 131
  val IINC: Int = 132
  val I2L: Int = 133
  val I2F: Int = 134
  val I2D: Int = 135
  val L2I: Int = 136
  val L2F: Int = 137
  val L2D: Int = 138
  val F2I: Int = 139
  val F2L: Int = 140
  val F2D: Int = 141
  val D2I: Int = 142
  val D2L: Int = 143
  val D2F: Int = 144
  val I2B: Int = 145
  val I2C: Int = 146
  val I2S: Int = 147
  val LCMP: Int = 148
  val FCMPL: Int = 149
  val FCMPG: Int = 150
  val DCMPL: Int = 151
  val DCMPG: Int = 152
  val IFEQ: Int = 153
  val IFNE: Int = 154
  val IFLT: Int = 155
  val IFGE: Int = 156
  val IFGT: Int = 157
  val IFLE: Int = 158
  val IF_ICMPEQ: Int = 159
  val IF_ICMPNE: Int = 160
  val IF_ICMPLT: Int = 161
  val IF_ICMPGE: Int = 162
  val IF_ICMPGT: Int = 163
  val IF_ICMPLE: Int = 164
  val IF_ACMPEQ: Int = 165
  val IF_ACMPNE: Int = 166
  val GOTO: Int = 167
  val JSR: Int = 168
  val RET: Int = 169
  val TABLESWITCH: Int = 170
  val LOOKUPSWITCH: Int = 171
  val IRETURN: Int = 172
  val LRETURN: Int = 173
  val FRETURN: Int = 174
  val DRETURN: Int = 175
  val ARETURN: Int = 176
  val RETURN: Int = 177

  // Instruction.Field
  val GETSTATIC: Int = 178
  val PUTSTATIC: Int = 179
  val GETFIELD: Int = 180
  val PUTFIELD: Int = 181

  // Instruction.Method
  val INVOKEVIRTUAL: Int = 182
  val INVOKESPECIAL: Int = 183
  val INVOKESTATIC: Int = 184
  val INVOKEINTERFACE: Int = 185

  val INVOKEDYNAMIC: Int = 186
  val NEW: Int = 187
  val NEWARRAY: Int = 188
  val ANEWARRAY: Int = 189
  val ARRAYLENGTH: Int = 190
  val ATHROW: Int = 191
  val CHECKCAST: Int = 192
  val INSTANCEOF: Int = 193
  val MONITORENTER: Int = 194
  val MONITOREXIT: Int = 195
  val MULTIANEWARRAY: Int = 197
  val IFNULL: Int = 198
  val IFNONNULL: Int = 199

  val LDC_W: Int = 19;
  val LDC2_W: Int = 20;
  val ILOAD_0: Int = 26;
  val ILOAD_1: Int = 27;
  val ILOAD_2: Int = 28;
  val ILOAD_3: Int = 29;
  val LLOAD_0: Int = 30;
  val LLOAD_1: Int = 31;
  val LLOAD_2: Int = 32;
  val LLOAD_3: Int = 33;
  val FLOAD_0: Int = 34;
  val FLOAD_1: Int = 35;
  val FLOAD_2: Int = 36;
  val FLOAD_3: Int = 37;
  val DLOAD_0: Int = 38;
  val DLOAD_1: Int = 39;
  val DLOAD_2: Int = 40;
  val DLOAD_3: Int = 41;
  val ALOAD_0: Int = 42;
  val ALOAD_1: Int = 43;
  val ALOAD_2: Int = 44;
  val ALOAD_3: Int = 45;
  val ISTORE_0: Int = 59;
  val ISTORE_1: Int = 60;
  val ISTORE_2: Int = 61;
  val ISTORE_3: Int = 62;
  val LSTORE_0: Int = 63;
  val LSTORE_1: Int = 64;
  val LSTORE_2: Int = 65;
  val LSTORE_3: Int = 66;
  val FSTORE_0: Int = 67;
  val FSTORE_1: Int = 68;
  val FSTORE_2: Int = 69;
  val FSTORE_3: Int = 70;
  val DSTORE_0: Int = 71;
  val DSTORE_1: Int = 72;
  val DSTORE_2: Int = 73;
  val DSTORE_3: Int = 74;
  val ASTORE_0: Int = 75;
  val ASTORE_1: Int = 76;
  val ASTORE_2: Int = 77;
  val ASTORE_3: Int = 78;
  val WIDE: Int = 196;
  val GOTO_W: Int = 200;
  val JSR_W: Int = 201;

  def parse(bytes: Array[Byte], constantPool: ConstantPool): Seq[Instruction] =
    val instructions = Buffer.empty[Instruction]
    val reader = new ByteReader(bytes)
    var current = 0
    while current < bytes.size do
      reader.unsignedByte(current) match
        case NOP | ACONST_NULL | ICONST_M1 | ICONST_0 | ICONST_1 | ICONST_2 | ICONST_3 | ICONST_4 | ICONST_5 |
            LCONST_0 | LCONST_1 | FCONST_0 | FCONST_1 | FCONST_2 | DCONST_0 | DCONST_1 | IALOAD | LALOAD | FALOAD |
            DALOAD | AALOAD | BALOAD | CALOAD | SALOAD | IASTORE | LASTORE | FASTORE | DASTORE | AASTORE | BASTORE |
            CASTORE | SASTORE | POP | POP2 | DUP | DUP_X1 | DUP_X2 | DUP2 | DUP2_X1 | DUP2_X2 | SWAP | IADD | LADD |
            FADD | DADD | ISUB | LSUB | FSUB | DSUB | IMUL | LMUL | FMUL | DMUL | IDIV | LDIV | FDIV | DDIV | IREM |
            LREM | FREM | DREM | INEG | LNEG | FNEG | DNEG | ISHL | LSHL | ISHR | LSHR | IUSHR | LUSHR | IAND | LAND |
            IOR | LOR | IXOR | LXOR | I2L | I2F | I2D | L2I | L2F | L2D | F2I | F2L | F2D | D2I | D2L | D2F | I2B |
            I2C | I2S | LCMP | FCMPL | FCMPG | DCMPL | DCMPG | IRETURN | LRETURN | FRETURN | DRETURN | ARETURN |
            RETURN | ARRAYLENGTH | ATHROW | MONITORENTER | MONITOREXIT | ILOAD_0 | ILOAD_1 | ILOAD_2 | ILOAD_3 |
            LLOAD_0 | LLOAD_1 | LLOAD_2 | LLOAD_3 | FLOAD_0 | FLOAD_1 | FLOAD_2 | FLOAD_3 | DLOAD_0 | DLOAD_1 |
            DLOAD_2 | DLOAD_3 | ALOAD_0 | ALOAD_1 | ALOAD_2 | ALOAD_3 | ISTORE_0 | ISTORE_1 | ISTORE_2 | ISTORE_3 |
            LSTORE_0 | LSTORE_1 | LSTORE_2 | LSTORE_3 | FSTORE_0 | FSTORE_1 | FSTORE_2 | FSTORE_3 | DSTORE_0 |
            DSTORE_1 | DSTORE_2 | DSTORE_3 | ASTORE_0 | ASTORE_1 | ASTORE_2 | ASTORE_3 =>
          current += 1
        case IFEQ | IFNE | IFLT | IFGE | IFGT | IFLE | IF_ICMPEQ | IF_ICMPNE | IF_ICMPLT | IF_ICMPGE | IF_ICMPGT |
            IF_ICMPLE | IF_ACMPEQ | IF_ACMPNE | GOTO | JSR | IFNULL | IFNONNULL =>
          current += 3
        case GOTO_W | JSR_W =>
          current += 5
        case WIDE =>
          reader.unsignedByte(current + 1) match
            case ILOAD | FLOAD | ALOAD | LLOAD | DLOAD | ISTORE | FSTORE | ASTORE | LSTORE | DSTORE | RET =>
              current += 4
            case IINC => current += 6
            case byte => reader.unexpected(current + 1)
        case TABLESWITCH =>
          current += 4 - (current & 3)
          val casesCount = reader.int(current + 8) - reader.int(current + 4) + 1
          current += 12 + 4 * casesCount
        case LOOKUPSWITCH =>
          current += 4 - (current & 3)
          val casesCount = reader.int(current + 4)
          current += 8 + 8 * casesCount
        case ILOAD | LLOAD | FLOAD | DLOAD | ALOAD | ISTORE | LSTORE | FSTORE | DSTORE | ASTORE | RET | BIPUSH |
            NEWARRAY | LDC =>
          current += 2
        case SIPUSH | LDC_W | LDC2_W | NEW | ANEWARRAY | CHECKCAST | INSTANCEOF | IINC =>
          current += 3
        case opcode @ (GETSTATIC | PUTSTATIC | GETFIELD | PUTFIELD) =>
          instructions += constantPool.readField(opcode, reader.unsignedShort(current + 1))
          current += 3
        case opcode @ (INVOKEVIRTUAL | INVOKESPECIAL | INVOKESTATIC) =>
          instructions += constantPool.readMethod(opcode, reader.unsignedShort(current + 1))
          current += 3
        case opcode @ INVOKEINTERFACE =>
          instructions += constantPool.readMethod(opcode, reader.unsignedShort(current + 1))
          current += 5
        case INVOKEDYNAMIC =>
          current += 5
        case MULTIANEWARRAY =>
          current += 4
        case tag => reader.unexpected(current)
    end while
    instructions.toSeq
