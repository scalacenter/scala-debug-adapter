package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi._
import RuntimeEvaluatorExtractors.PrimitiveTest.*

sealed trait RuntimeBinaryOp {
  def evaluate(lhs: JdiValue, rhs: JdiValue, loader: JdiClassLoader): Safe[JdiValue]
  def typeCheck(lhs: Type, rhs: Type): Type
}

sealed trait RuntimeUnaryOp {
  def evaluate(rhs: JdiValue, loader: JdiClassLoader): Safe[JdiValue]
  def typeCheck(lhs: Type): Type
}

sealed trait NumericUnaryOp extends RuntimeUnaryOp {
  override def typeCheck(lhs: Type): Type = lhs match {
    case IsNumeric() => lhs
    case _ => throw new IllegalArgumentException(s"Unexpected type $lhs")
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

case object UnaryPlus extends NumericUnaryOp {
  override def evaluate(rhs: JdiValue, loader: JdiClassLoader): Safe[JdiValue] = Safe(rhs)
}
case object UnaryMinus extends NumericUnaryOp {
  override def evaluate(rhs: JdiValue, loader: JdiClassLoader): Safe[JdiValue] =
    for {
      unboxed <- rhs.unboxIfPrimitive
      result <- unboxed match {
        case double: DoubleValue => Safe(loader.mirrorOf(-double.doubleValue()))
        case float: FloatValue => Safe(loader.mirrorOf(-float.floatValue()))
        case long: LongValue => Safe(loader.mirrorOf(-long.longValue()))
        case int: IntegerValue => Safe(loader.mirrorOf(-int.intValue()))
        case short: ShortValue => Safe(loader.mirrorOf(-short.shortValue()))
        case byte: ByteValue => Safe(loader.mirrorOf(-byte.byteValue()))
        case _ => Safe.failed(s"Unexpected type ${unboxed.value.`type`}")
      }
    } yield result

}
case object UnaryBitwiseNot extends RuntimeUnaryOp {
  override def typeCheck(lhs: Type): Type = lhs match {
    case IsIntegral() => lhs
    case _ => throw new IllegalArgumentException(s"Unexpected type $lhs")
  }

  override def evaluate(rhs: JdiValue, loader: JdiClassLoader): Safe[JdiValue] =
    for {
      unboxed <- rhs.unboxIfPrimitive
      result <- unboxed.value match {
        case long: LongValue => Safe(loader.mirrorOf(~long.longValue()))
        case int: IntegerValue => Safe(loader.mirrorOf(~int.intValue()))
        case short: ShortValue => Safe(loader.mirrorOf(~short.shortValue()))
        case byte: ByteValue => Safe(loader.mirrorOf(~byte.byteValue()))
        case _ => Safe.failed(s"Unexpected type ${unboxed.value.`type`}")
      }
    } yield result
}

/* -------------------------------------------------------------------------- */
/*                         Primitive binary operations                        */
/* -------------------------------------------------------------------------- */
object RuntimeBinaryOp {
  def apply(
      lhs: RuntimeEvaluableTree,
      rhs: RuntimeEvaluableTree,
      op: String
  ): Validation[RuntimeBinaryOp] = {
    (lhs.`type`, rhs.`type`, op) match {
      case (_, _, "==") => Valid(Eq)
      case (_, _, "!=") => Valid(Neq)
      case (NotPrimitive(), _, _) | (_, NotPrimitive(), _) =>
        Recoverable("Primitive operations don't support reference types")
      case (IsBoolean(), IsBoolean(), "&&") => Valid(And)
      case (IsBoolean(), IsBoolean(), "||") => Valid(Or)
      case (_, _, "&&") | (_, _, "||") =>
        Recoverable("Boolean operations don't support numeric types")
      case (NotNumeric(), _, _) | (_, NotNumeric(), _) =>
        Recoverable("Numeric operations don't support boolean types")
      case (_, _, "+") => Valid(Plus)
      case (_, _, "-") => Valid(Minus)
      case (_, _, "*") => Valid(Times)
      case (_, _, "/") => Valid(Div)
      case (_, _, "%") => Valid(Modulo)
      case (_, _, "<") => Valid(Less)
      case (_, _, "<=") => Valid(LessOrEqual)
      case (_, _, ">") => Valid(Greater)
      case (_, _, ">=") => Valid(GreaterOrEqual)
      case _ => Recoverable("Not a primitive binary operation")
    }
  }

  def apply(
      lhs: RuntimeTree,
      rhs: Seq[RuntimeEvaluableTree],
      op: String
  ): Validation[RuntimeBinaryOp] =
    (lhs, rhs) match {
      case (left: RuntimeEvaluableTree, Seq(right)) => apply(left, right, op)
      case _ => Recoverable("Not a primitive binary operation")
    }
}

/* --------------------------- Numeric operations --------------------------- */
sealed trait NumericOp extends RuntimeBinaryOp {
  private def primitiveTypeCheck(lhs: Type, rhs: Type): Option[PrimitiveType] =
    (lhs, rhs) match {
      case (d: DoubleType, _) => Some(d)
      case (_, d: DoubleType) => Some(d)
      case (f: FloatType, _) => Some(f)
      case (_, f: FloatType) => Some(f)
      case (l: LongType, _) => Some(l)
      case (_, l: LongType) => Some(l)
      case (i: IntegerType, _) => Some(i)
      case (_, i: IntegerType) => Some(i)
      case (s: ShortType, _) => Some(s)
      case (_, s: ShortType) => Some(s)
      case (b: ByteType, _) => Some(b)
      case (_, b: ByteType) => Some(b)
      case (c: CharType, _) => Some(c)
      case (_, c: CharType) => Some(c)
      case _ => None
    }

  private def referenceTypeCheck(lhs: Type, rhs: Type): Option[Type] = {
    (lhs.name(), rhs.name()) match {
      case ("java.lang.Double", _) => Some(lhs)
      case (_, "java.lang.Double") => Some(rhs)
      case ("java.lang.Float", _) => Some(lhs)
      case (_, "java.lang.Float") => Some(rhs)
      case ("java.lang.Long", _) => Some(lhs)
      case (_, "java.lang.Long") => Some(rhs)
      case ("java.lang.Integer", _) => Some(lhs)
      case (_, "java.lang.Integer") => Some(rhs)
      case ("java.lang.Short", _) => Some(lhs)
      case (_, "java.lang.Short") => Some(rhs)
      case ("java.lang.Byte", _) => Some(lhs)
      case (_, "java.lang.Byte") => Some(rhs)
      case ("java.lang.Character", _) => Some(lhs)
      case (_, "java.lang.Character") => Some(rhs)
      case _ => None
    }
  }

  override def typeCheck(lhs: Type, rhs: Type): Type =
    primitiveTypeCheck(lhs, rhs).orElse(referenceTypeCheck(lhs, rhs)) match {
      case Some(t) => t
      case None => throw new IllegalArgumentException(s"Unexpected types $lhs and $rhs")
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
      case Some(value) => Safe.successful(clsLoader.mirrorOfAnyVal(value))
      case None => Safe.failed(MethodInvocationFailed("Division by zero", None))
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
      case Some(value) => Safe.successful(clsLoader.mirrorOfAnyVal(value))
      case None => Safe.failed(MethodInvocationFailed("Division by zero", None))
    }
  }

  def evaluate(lhs: JdiValue, rhs: JdiValue, loader: JdiClassLoader) = {
    typeCheck(lhs.value.`type`(), rhs.value.`type`()) match {
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
      case _ => Safe.failed(new IllegalArgumentException("Invalid numeric types"))
    }
  }
}

/* --------------------------- Boolean operations --------------------------- */
sealed trait BooleanOp extends RuntimeBinaryOp {
  override def typeCheck(lhs: Type, rhs: Type): PrimitiveType =
    lhs.virtualMachine().mirrorOf(true).`type`().asInstanceOf[BooleanType]

  override def evaluate(lhs: JdiValue, rhs: JdiValue, loader: JdiClassLoader): Safe[JdiValue] =
    for {
      l <- lhs.toBoolean
      r <- rhs.toBoolean
    } yield this match {
      case And => loader.mirrorOf(l && r)
      case Or => loader.mirrorOf(l || r)
    }
}

case object And extends BooleanOp
case object Or extends BooleanOp

/* --------------------------- Object operations --------------------------- */
sealed trait ObjectOp extends RuntimeBinaryOp {
  override def typeCheck(lhs: Type, rhs: Type): PrimitiveType =
    lhs.virtualMachine().mirrorOf(true).`type`().asInstanceOf[PrimitiveType]

  override def evaluate(
      lhs: JdiValue,
      rhs: JdiValue,
      loader: JdiClassLoader
  ): Safe[JdiValue] =
    for {
      boxesRuntime <- loader.loadClass("scala.runtime.BoxesRunTime")
      boxed <- Seq(lhs, rhs).map(loader.boxIfPrimitive).traverse
      eqResult <- boxesRuntime.invokeStatic("equals", "(Ljava/lang/Object;Ljava/lang/Object;)Z", boxed)
    } yield this match {
      case Eq => eqResult
      case Neq => loader.mirrorOf(!eqResult.value.asInstanceOf[BooleanValue].value())
    }
}
case object Eq extends ObjectOp
case object Neq extends ObjectOp

/* -------------------------------------------------------------------------- */
/*                         Primitive unary operations                         */
/* -------------------------------------------------------------------------- */
case object Not extends RuntimeUnaryOp {
  override def evaluate(rhs: JdiValue, loader: JdiClassLoader) =
    rhs.toBoolean.map(v => loader.mirrorOf(!v))
  override def typeCheck(rhs: Type): Type = rhs.virtualMachine().mirrorOf(true).`type`().asInstanceOf[BooleanType]
}

object RuntimeUnaryOp {
  def apply(rhs: RuntimeEvaluableTree, op: String): Validation[RuntimeUnaryOp] = {
    (rhs.`type`, op) match {
      case (IsNumeric(), "unary_+") => Valid(UnaryPlus)
      case (IsNumeric(), "unary_-") => Valid(UnaryMinus)
      case (IsIntegral(), "unary_~") => Valid(UnaryBitwiseNot)
      case (IsBoolean(), "unary_!") => Valid(Not)
      case _ => Recoverable("Not a primitive unary operation")
    }
  }

  def apply(rhs: RuntimeTree, op: String): Validation[RuntimeUnaryOp] =
    rhs match {
      case ret: RuntimeEvaluableTree => apply(ret, op)
      case _ => Recoverable("Not a primitive unary operation")
    }
}
