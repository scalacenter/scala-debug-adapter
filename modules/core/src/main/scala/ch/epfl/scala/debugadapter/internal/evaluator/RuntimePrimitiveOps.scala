package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi.*

object RuntimePrimitiveOps {
  sealed trait BinaryOp {
    def evaluate(lhs: JdiValue, rhs: JdiValue, loader: JdiClassLoader): Safe[JdiValue]
    def typeCheck(lhs: Type, rhs: Type): Type
  }

  sealed trait UnaryOp {
    def evaluate(rhs: JdiValue, loader: JdiClassLoader): Safe[JdiValue]
    def typeCheck(lhs: Type): Type
  }

  sealed trait NumericUnaryOp extends UnaryOp {
    override def typeCheck(lhs: Type): Type =
      if (isNumeric(lhs)) lhs else throw new IllegalArgumentException(s"Unexpected type $lhs")
  }

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
  case object UnaryBitwiseNot extends UnaryOp {
    override def typeCheck(lhs: Type): Type =
      if (isIntegral(lhs)) lhs else throw new IllegalArgumentException(s"Unexpected type $lhs")

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

  object BinaryOp {
    def apply(lhs: Type, rhs: Type, op: String): Validation[BinaryOp] = {
      def notDefined = Recoverable(s"The $op operator is not defined on ${lhs.name} and ${rhs.name}")
      op match {
        case "==" => Valid(Eq)
        case "!=" => Valid(Neq)
        case _ if lhs == null || rhs == null => Recoverable(s"The $op operator is not defined on null value")
        case _ if !isPrimitive(lhs) || !isPrimitive(rhs) => notDefined
        case "&&" if isBoolean(lhs) && isBoolean(rhs) => Valid(And)
        case "||" if isBoolean(lhs) && isBoolean(rhs) => Valid(Or)
        case "&&" | "||" => notDefined
        case _ if !isNumeric(lhs) || !isNumeric(rhs) => notDefined
        case "+" => Valid(Plus)
        case "-" => Valid(Minus)
        case "*" => Valid(Times)
        case "/" => Valid(Div)
        case "%" => Valid(Modulo)
        case "<" => Valid(Less)
        case "<=" => Valid(LessOrEqual)
        case ">" => Valid(Greater)
        case ">=" => Valid(GreaterOrEqual)
        case _ => notDefined
      }
    }
  }

  /* --------------------------- Numeric operations --------------------------- */
  sealed trait NumericOp extends BinaryOp {
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
      (lhs.name, rhs.name) match {
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
      Safe(this)
        .map {
          case Plus => x + y
          case Minus => x - y
          case Times => x * y
          case Div => x / y
          case Modulo =>
            x match {
              case d: Double => d % y.asInstanceOf[Double]
              case f: Float => f % y.asInstanceOf[Float]
            }
          case Less => fractional.lt(x, y)
          case LessOrEqual => fractional.lteq(x, y)
          case Greater => fractional.gt(x, y)
          case GreaterOrEqual => fractional.gteq(x, y)
        }
        .map(clsLoader.mirrorOfAnyVal)
        .recoverWith { case e: ArithmeticException => Safe.failed(RuntimeException(e.getMessage, None)) }
    }

    private def computeIntegral[T <: AnyVal](x: T, y: T, clsLoader: JdiClassLoader)(implicit
        integral: Integral[T]
    ): Safe[JdiValue] = {
      import Integral.Implicits._
      Safe(this)
        .map {
          case Plus => x + y
          case Minus => x - y
          case Times => x * y
          case Div => x / y
          case Modulo => x % y
          case Less => integral.lt(x, y)
          case LessOrEqual => integral.lteq(x, y)
          case Greater => integral.gt(x, y)
          case GreaterOrEqual => integral.gteq(x, y)
        }
        .map(clsLoader.mirrorOfAnyVal)
        .recoverWith { case e: ArithmeticException => Safe.failed(RuntimeException(e.getMessage, None)) }
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

  /* --------------------------- Boolean operations --------------------------- */
  sealed trait BooleanOp extends BinaryOp {
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

  sealed trait ObjectOp extends BinaryOp {
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

  case object Not extends UnaryOp {
    override def evaluate(rhs: JdiValue, loader: JdiClassLoader) =
      rhs.toBoolean.map(v => loader.mirrorOf(!v))
    override def typeCheck(rhs: Type): Type = rhs.virtualMachine().mirrorOf(true).`type`().asInstanceOf[BooleanType]
  }

  object UnaryOp {
    def apply(rhs: Type, op: String): Validation[UnaryOp] =
      op match {
        case _ if rhs == null => Recoverable(s"$op is not defined on null")
        case "unary_+" if isNumeric(rhs) => Valid(UnaryPlus)
        case "unary_-" if isNumeric(rhs) => Valid(UnaryMinus)
        case "unary_~" if isIntegral(rhs) => Valid(UnaryBitwiseNot)
        case "unary_!" if isBoolean(rhs) => Valid(Not)
        case _ => Recoverable(s"$op is not defined on ${rhs.name}")
      }
  }

  /* -------------------------------------------------------------------------- */
  /*                          Allowed types reference                           */
  /* -------------------------------------------------------------------------- */
  private val integrals = Set("java.lang.Integer", "java.lang.Long", "java.lang.Short", "java.lang.Byte")
  private val fractionals = Set("java.lang.Float", "java.lang.Double")

  def isIntegral(x: Type): Boolean = x match {
    case _: ByteType | _: ShortType | _: CharType | _: IntegerType | _: LongType => true
    case _ if integrals.contains(x.name) => true
    case _ => false
  }

  def isFractional(x: Type): Boolean = x match {
    case _: FloatType | _: DoubleType => true
    case _ if fractionals.contains(x.name) => true
    case _ => false
  }

  def isNumeric(x: Type): Boolean = isIntegral(x) || isFractional(x) || x.name == "java.lang.Character"

  def isBoolean(x: Type): Boolean = x.isInstanceOf[BooleanType] || x.name == "java.lang.Boolean"

  def isPrimitive(x: Type): Boolean = isNumeric(x) || isBoolean(x)
}
