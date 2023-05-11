package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi._

sealed trait RuntimeBinaryOp {
  def evaluate(lhs: JdiValue, rhs: JdiValue, loader: JdiClassLoader): Safe[JdiValue]
  def typeCheck(lhs: Type, rhs: Type): PrimitiveType
}

//TODO: supports + - ~
sealed trait RuntimeUnaryOp {
  def evaluate(rhs: JdiValue, loader: JdiClassLoader): Safe[JdiValue]
  def typeCheck(lhs: Type): PrimitiveType
}

object RuntimeBinaryOp {
  val allowedNumericTypes = Set(
    "java.lang.Number",
    "java.lang.Integer",
    "java.lang.Long",
    "java.lang.Float",
    "java.lang.Double",
    "java.lang.Short",
    "java.lang.Byte",
    "java.lang.Character"
  )
  val allowedBooleanTypes = Set("java.lang.Boolean")
  val allowedReferenceTypes = allowedNumericTypes ++ allowedBooleanTypes

  // TODO: type check in the operator
  def apply(
      lhs: RuntimeEvaluationTree,
      rhs: RuntimeEvaluationTree,
      op: String
  ): Validation[RuntimeBinaryOp] = {
    (lhs.`type`, rhs.`type`, op) match {
      case (_, _, "==") => Valid(Eq)
      case (_, _, "!=") => Valid(Neq)
      case (_: ReferenceType, _, _) if !allowedReferenceTypes.contains(lhs.`type`.name()) =>
        Recoverable("Primitive operations don't support reference types")
      case (_, _: ReferenceType, _) if !allowedReferenceTypes.contains(rhs.`type`.name()) =>
        Recoverable("Primitive operations don't support reference types")
      case (_, _, "&&") => And(lhs.`type`, rhs.`type`)
      case (_, _, "||") => Or(lhs.`type`, rhs.`type`)
      case (_: ReferenceType, _, _) if allowedBooleanTypes.contains(lhs.`type`.name()) =>
        Recoverable("Numeric operations don't support boolean types")
      case (_, _: ReferenceType, _) if allowedBooleanTypes.contains(rhs.`type`.name()) =>
        Recoverable("Numeric operations don't support boolean types")
      case (_: BooleanType, _, _) | (_, _: BooleanType, _) =>
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

  def fromValidations(
      lhs: Validation[RuntimeTree],
      rhs: Validation[Seq[RuntimeEvaluationTree]],
      op: String
  ): Validation[RuntimeBinaryOp] = {
    (lhs, rhs) match {
      case (Valid(left: RuntimeEvaluationTree), Valid(Seq(right))) => apply(left, right, op)
      case _ => Recoverable("Not a primitive binary operation")
    }
  }

  def getBinaryOpTree(
      lhs: Validation[RuntimeTree],
      rhs: Validation[Seq[RuntimeEvaluationTree]],
      op: String
  ): Validation[PrimitiveBinaryOpTree] =
    (lhs, rhs) match {
      case (Valid(left: RuntimeEvaluationTree), Valid(Seq(right))) =>
        fromValidations(lhs, rhs, op).map(PrimitiveBinaryOpTree(left, right, _))
      case _ => Recoverable("Not a primitive binary operation")
    }
}

object RuntimeUnaryOp {
  val booleanOperation = Set("unary_!")
  val allowedReferenceTypes = Set("java.lang.Boolean")

  def apply(rhs: RuntimeEvaluationTree, op: String): Validation[RuntimeUnaryOp] = {
    op match {
      case "unary_!" if allowedReferenceTypes.contains(rhs.`type`.name) => Valid(Not)
      case "unary_!" if rhs.`type`.isInstanceOf[BooleanType] => Valid(Not)
      case _ => Recoverable("Not a primitive unary operation")
    }
  }

  def getUnaryOpTree(
      rhs: Validation[RuntimeTree],
      op: String
  ): Validation[PrimitiveUnaryOpTree] = rhs match {
    case Valid(rt: RuntimeEvaluationTree) => apply(rt, op).map(PrimitiveUnaryOpTree(rt, _))
    case _ => Recoverable("Not a primitive unary operation")
  }
}

sealed trait NumericOp extends RuntimeBinaryOp {
  override def typeCheck(lhs: Type, rhs: Type): PrimitiveType =
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

case object Plus extends NumericOp
case object Minus extends NumericOp
case object Times extends NumericOp
case object Div extends NumericOp
case object Modulo extends NumericOp
case object Less extends NumericOp
case object LessOrEqual extends NumericOp
case object Greater extends NumericOp
case object GreaterOrEqual extends NumericOp

sealed trait BooleanOp extends RuntimeBinaryOp {
  override def typeCheck(lhs: Type, rhs: Type): PrimitiveType =
    lhs.virtualMachine().mirrorOf(true).`type`().asInstanceOf[BooleanType]
  def apply(lhs: Type, rhs: Type): Validation[BooleanOp] =
    (lhs, rhs) match {
      case (b: BooleanType, _: BooleanType) => Valid(this)
      case (b: BooleanType, ref: ReferenceType) if RuntimeBinaryOp.allowedBooleanTypes.contains(ref.name()) =>
        Valid(this)
      case (ref: ReferenceType, b: BooleanType) if RuntimeBinaryOp.allowedBooleanTypes.contains(ref.name()) =>
        Valid(this)
      case (ref1: ReferenceType, ref2: ReferenceType) =>
        if (
          RuntimeBinaryOp.allowedBooleanTypes
            .contains(ref1.name()) && RuntimeBinaryOp.allowedBooleanTypes.contains(ref2.name())
        )
          Valid(this)
        else Recoverable("Not a boolean primitive operation")
    }
}
case object And extends BooleanOp {
  override def evaluate(lhs: JdiValue, rhs: JdiValue, loader: JdiClassLoader) =
    for {
      l <- lhs.toBoolean
      r <- rhs.toBoolean
    } yield loader.mirrorOf(l && r)
}
case object Or extends BooleanOp {
  override def evaluate(lhs: JdiValue, rhs: JdiValue, loader: JdiClassLoader) =
    for {
      l <- lhs.toBoolean
      r <- rhs.toBoolean
    } yield loader.mirrorOf(l || r)
}
case object Not extends RuntimeUnaryOp {
  override def evaluate(rhs: JdiValue, loader: JdiClassLoader) =
    rhs.toBoolean.map(v => loader.mirrorOf(!v))
  override def typeCheck(rhs: Type) = rhs.virtualMachine().mirrorOf(true).`type`().asInstanceOf[BooleanType]
}

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
