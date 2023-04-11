package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi._
import scala.util.Failure

sealed trait RuntimePrimitiveOp {
  def evaluate(lhs: JdiValue, rhs: JdiValue, loader: JdiClassLoader): Safe[JdiValue]
  def typeWith(lhs: Type, rhs: Type): PrimitiveType
}

object RuntimePrimitiveOp {
  val numericOperations = Set("+", "-", "*", "/", "%", "<", ">", "<=", ">=")
  val objectOperations = Set("==", "!=")
  val booleanOperations = Set("&&", "||", "!")
  val allOperations = numericOperations ++ objectOperations ++ booleanOperations

  val allowedReferenceTypes = Set(
    "java.lang.Integer",
    "java.lang.Long",
    "java.lang.Float",
    "java.lang.Double",
    "java.lang.Short",
    "java.lang.Byte",
    "java.lang.Character",
    "java.lang.Boolean"
  )

  // TODO: 1 - add support for types that inherit from the types above
  // TODO: 2 - add some type checking (do 1 before)
  def apply(lhs: RuntimeValidationTree, rhs: RuntimeValidationTree, op: String): Option[RuntimePrimitiveOp] = {
    (lhs.`type`, rhs.`type`, op) match {
      case (_, _, "==") => Some(Eq)
      case (_, _, "!=") => Some(Neq)
      case (_: ReferenceType, _, _) if !allowedReferenceTypes.contains(lhs.`type`.name()) => None
      case (_, _: ReferenceType, _) if !allowedReferenceTypes.contains(rhs.`type`.name()) => None
      case (_, _, "&&") => Some(And)
      case (_, _, "||") => Some(Or)
      case (_, _, "!") => Some(Not)
      case (_, _, "+") => Some(Plus)
      case (_, _, "-") => Some(Minus)
      case (_, _, "*") => Some(Times)
      case (_, _, "/") => Some(Div)
      case (_, _, "%") => Some(Modulo)
      case (_, _, "<") => Some(Less)
      case (_, _, "<=") => Some(LessOrEqual)
      case (_, _, ">") => Some(Greater)
      case (_, _, ">=") => Some(GreaterOrEqual)
      case _ => None
    }
  }
}

sealed trait NumericOp extends RuntimePrimitiveOp {
  def typeWith(lhs: Type, rhs: Type): PrimitiveType =
    (lhs, rhs) match {
      case (d: DoubleType, _) => d
      case (_, d: DoubleType) => d
      case (f: FloatType, _) => f
      case (_, f: FloatType) => f
      case (l: LongType, _) => l
      case (_, l: LongType) => l
      case (i: IntegerType, _) => i
      case (_, i: IntegerType) => i
      case (s: ShortType, _) => s
      case (_, s: ShortType) => s
      case (b: ByteType, _) => b
      case (_, b: ByteType) => b
      case (c: CharType, _) => c
      case (_, c: CharType) => c
    }

  private def computeFractional[T <: AnyVal](x: T, y: T, clsLoader: JdiClassLoader)(implicit
      fractional: Fractional[T]
  ): Safe[JdiValue] = {
    import Fractional.Implicits._
    val result: Option[AnyVal] = this match {
      case Plus => Some(x + y)
      case Minus => Some(x - y)
      case Times => Some(x * y)
      case Div if y != 0 => Some(x / y)
      case Modulo if y != 0 =>
        x match {
          case d: Double => Some(d % y.asInstanceOf[Double])
          case f: Float => Some(f % y.asInstanceOf[Float])
        }
      case Div | Modulo => None
      case Less => Some(fractional.lt(x, y))
      case LessOrEqual => Some(fractional.lteq(x, y))
      case Greater => Some(fractional.gt(x, y))
      case GreaterOrEqual => Some(fractional.gteq(x, y))
    }

    result match {
      case Some(value) => clsLoader.mirrorOfAnyVal(value)
      case None => Safe(Failure(new IllegalArgumentException("Division by zero")))
    }
  }

  private def computeIntegral[T <: AnyVal](x: T, y: T, clsLoader: JdiClassLoader)(implicit
      integral: Integral[T]
  ): Safe[JdiValue] = {
    import Integral.Implicits._
    val result: Option[AnyVal] = this match {
      case Plus => Some(x + y)
      case Minus => Some(x - y)
      case Times => Some(x * y)
      case Div if y != 0 => Some(x / y)
      case Modulo if y != 0 => Some(x % y)
      case Div | Modulo => None
      case Less => Some(integral.lt(x, y))
      case LessOrEqual => Some(integral.lteq(x, y))
      case Greater => Some(integral.gt(x, y))
      case GreaterOrEqual => Some(integral.gteq(x, y))
    }

    result match {
      case Some(value) => clsLoader.mirrorOfAnyVal(value)
      case None => throw new IllegalArgumentException("Division by zero")
    }
  }

  def evaluate(lhs: JdiValue, rhs: JdiValue, loader: JdiClassLoader): Safe[JdiValue] = {
    typeWith(lhs.value.`type`(), rhs.value.`type`()) match {
      case _: DoubleType =>
        for {
          l <- lhs.toDouble
          r <- rhs.toDouble
          result <- computeFractional(l, r, loader)
        } yield result
      case _: FloatType =>
        for {
          l <- lhs.toFloat
          r <- rhs.toFloat
          result <- computeFractional(l, r, loader)
        } yield result
      case _: LongType =>
        for {
          l <- lhs.toLong
          r <- rhs.toLong
          result <- computeIntegral(l, r, loader)
        } yield result
      case _: IntegerType =>
        for {
          l <- lhs.toInt
          r <- rhs.toInt
          result <- computeIntegral(l, r, loader)
        } yield result
      case _: ShortType =>
        for {
          l <- lhs.toShort
          r <- rhs.toShort
          result <- computeIntegral(l, r, loader)
        } yield result
      case _: ByteType =>
        for {
          l <- lhs.toByte
          r <- rhs.toByte
          result <- computeIntegral(l, r, loader)
        } yield result
      case _: CharType =>
        for {
          l <- lhs.toChar
          r <- rhs.toChar
          result <- computeIntegral(l, r, loader)
        } yield result
      case _ => throw new IllegalArgumentException(s"Cannot cast $lhs and $rhs to a numeric value")
    }
  }
}

case object Plus extends NumericOp
case object Minus extends NumericOp
case object Times extends NumericOp
case object Div extends NumericOp
case object Modulo extends NumericOp
case object Less extends NumericOp
case object LessOrEqual extends NumericOp
case object Greater extends NumericOp
case object GreaterOrEqual extends NumericOp

sealed trait BooleanOp extends RuntimePrimitiveOp {
  def typeWith(lhs: Type, rhs: Type): BooleanType =
    lhs.asInstanceOf[BooleanType]
  override def evaluate(
      lhs: JdiValue,
      rhs: JdiValue,
      loader: JdiClassLoader
  ): Safe[JdiValue] =
    for {
      l <- lhs.toBoolean
      r <- rhs.toBoolean
      result <- this match {
        case And => loader.mirrorOf(l && r)
        case Or => loader.mirrorOf(l || r)
        case Not => loader.mirrorOf(!l)
      }
    } yield result
}
case object And extends BooleanOp
case object Or extends BooleanOp
case object Not extends BooleanOp

sealed trait ObjectOp extends RuntimePrimitiveOp {
  override def typeWith(lhs: Type, rhs: Type): PrimitiveType =
    lhs.virtualMachine().mirrorOf(true).`type`().asInstanceOf[PrimitiveType]

  override def evaluate(
      lhs: JdiValue,
      rhs: JdiValue,
      loader: JdiClassLoader
  ): Safe[JdiValue] = this match {
    case Eq => loader.mirrorOf(lhs.value == rhs.value)
    case Neq => loader.mirrorOf(!(lhs.value == rhs.value))
  }
}
case object Eq extends ObjectOp
case object Neq extends ObjectOp
