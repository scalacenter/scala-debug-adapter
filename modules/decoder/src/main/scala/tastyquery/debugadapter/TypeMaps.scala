package tastyquery.debugadapter

import scala.collection.mutable

import tastyquery.Annotations.*
import tastyquery.Contexts.*
import tastyquery.Flags.*
import tastyquery.Names.*
import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Types.*

private[debugadapter] object TypeMaps:

  /** Common base class of TypeMap and TypeAccumulator */
  abstract class VariantTraversal:
    protected var variance: Int = 1

    inline protected def atVariance[T](v: Int)(op: => T): T =
      val saved = variance
      variance = v
      val res = op
      variance = saved
      res
  end VariantTraversal

  abstract class TypeMap extends VariantTraversal:
    thisMap =>

    final def apply(tp: TypeMappable): tp.ThisTypeMappableType =
      transform(tp).asInstanceOf[tp.ThisTypeMappableType]

    protected def transform(tp: TypeMappable): TypeMappable

    protected def derivedSelect(tp: NamedType, pre: Type): Type =
      tp.derivedSelect(pre)
    protected def derivedRecType(tp: RecType, parent: Type): RecType =
      tp.derivedRecType(parent)
    protected def derivedTypeRefinement(tp: TypeRefinement, parent: Type, refinedBounds: TypeBounds): Type =
      tp.derivedTypeRefinement(parent, tp.refinedName, refinedBounds)
    protected def derivedTermRefinement(tp: TermRefinement, parent: Type, refinedType: TypeOrMethodic): Type =
      tp.derivedTermRefinement(parent, tp.refinedName, refinedType)
    protected def derivedWildcardTypeArg(tp: WildcardTypeArg, bounds: TypeBounds): WildcardTypeArg =
      tp.derivedWildcardTypeArg(bounds)
    protected def derivedAppliedType(tp: AppliedType, tycon: Type, args: List[TypeOrWildcard]): Type =
      tp.derivedAppliedType(tycon, args)
    protected def derivedAndType(tp: AndType, tp1: Type, tp2: Type): Type =
      tp.derivedAndType(tp1, tp2)
    protected def derivedOrType(tp: OrType, tp1: Type, tp2: Type): Type =
      tp.derivedOrType(tp1, tp2)
    protected def derivedAnnotatedType(tp: AnnotatedType, underlying: Type, annot: Annotation): Type =
      tp.derivedAnnotatedType(underlying, annot)
    protected def derivedMatchType(tp: MatchType, bound: Type, scrutinee: Type, cases: List[MatchTypeCase]): Type =
      tp.derivedMatchType(bound, scrutinee, cases)
    protected def derivedByNameType(tp: ByNameType, restpe: Type): Type =
      tp.derivedByNameType(restpe)
    protected def derivedRepeatedType(tp: RepeatedType, elemType: Type): Type =
      tp.derivedRepeatedType(elemType)
    protected def derivedLambdaType(tp: LambdaType, formals: List[tp.PInfo], restpe: tp.ResultType): tp.This =
      tp.derivedLambdaType(tp.paramNames, formals, restpe)
    protected def derivedSkolemType(tp: SkolemType, tpe: Type): SkolemType =
      tp.derivedSkolemType(tpe)

    protected def derivedTypeAlias(tp: TypeAlias, alias: Type): TypeBounds =
      tp.derivedTypeAlias(alias)
    protected def derivedTypeBounds(bounds: TypeBounds, low: Type, high: Type): TypeBounds =
      bounds.derivedTypeBounds(low, high)

    protected def mapOverLambda(tp: LambdaType): tp.This =
      val restpe = tp.resultType
      val saved = variance
      variance = -variance
      val ptypes1 = tp.paramInfos.mapConserve(pi => apply(pi)).asInstanceOf[List[tp.PInfo]]
      variance = saved
      derivedLambdaType(tp, ptypes1, this(restpe).asInstanceOf[tp.ResultType])

    protected def mapOverMatchTypeCase(caze: MatchTypeCase): MatchTypeCase =
      caze.derivedMatchTypeCase(
        caze.paramTypeBounds.mapConserve(bounds => apply(bounds)),
        this(caze.pattern),
        this(caze.result)
      )
    end mapOverMatchTypeCase

    def isRange(tp: TypeOrWildcard): Boolean = tp.isInstanceOf[Range]

    final def mapOver(tp: TypeMappable): tp.ThisTypeMappableType =
      val result: TypeMappable = tp match
        case tp: Type => mapOver(tp): tp.ThisTypeMappableType
        case tp: TypeBounds => mapOver(tp): tp.ThisTypeMappableType
        case tp: LambdaType => mapOverLambda(tp): tp.ThisTypeMappableType
        case tp: WildcardTypeArg => derivedWildcardTypeArg(tp, this(tp.bounds)): tp.ThisTypeMappableType
        case tp @ NoPrefix => tp: tp.ThisTypeMappableType
        case tp: PackageRef => tp: tp.ThisTypeMappableType

      result.asInstanceOf[tp.ThisTypeMappableType]
    end mapOver

    /** Map this function over given type */
    def mapOver(tp: Type): Type =
      tp match
        case tp: NamedType =>
          // A prefix is never contravariant. Even if say `p.A` is used in a contravariant
          // context, we cannot assume contravariance for `p` because `p`'s lower
          // bound might not have a binding for `A` (e.g. the lower bound could be `Nothing`).
          // By contrast, covariance does translate to the prefix, since we have that
          // if `p <: q` then `p.A <: q.A`, and well-formedness requires that `A` is a member
          // of `p`'s upper bound.
          tp.prefix match
            case NoPrefix | _: PackageRef =>
              tp
            case prefix: Type =>
              val prefix1 = atVariance(variance max 0)(this(prefix))
              derivedSelect(tp, prefix1)

        case tp: AppliedType =>
          tp.map(this(_), this(_))

        case _: ThisType | _: SuperType | _: ConstantType | _: BoundType | _: NothingType | _: AnyKindType =>
          tp

        case tp: TypeLambda =>
          mapOverLambda(tp)

        case tp: ByNameType =>
          derivedByNameType(tp, this(tp.resultType))

        case tp: RepeatedType =>
          derivedRepeatedType(tp, this(tp.elemType))

        case tp: AnnotatedType =>
          derivedAnnotatedType(tp, this(tp.typ), tp.annotation) // tp.annotation.mapWith(this)

        case tp: RecType =>
          derivedRecType(tp, this(tp.parent))

        case tp: TypeRefinement =>
          derivedTypeRefinement(tp, this(tp.parent), this(tp.refinedBounds))

        case tp: TermRefinement =>
          derivedTermRefinement(tp, this(tp.parent), this(tp.refinedType))

        case tp: AndType =>
          derivedAndType(tp, this(tp.first), this(tp.second))

        case tp: OrType =>
          derivedOrType(tp, this(tp.first), this(tp.second))

        case tp: MatchType =>
          // The spec says that all type positions in a match type are considered invariant
          atVariance(0) {
            val newBound = this(tp.bound)
            val newScrutinee = this(tp.scrutinee)
            val newCases = tp.cases.mapConserve(mapOverMatchTypeCase(_))
            derivedMatchType(tp, newBound, newScrutinee, newCases)
          }

        case tp: SkolemType =>
          derivedSkolemType(tp, this(tp.tpe))

        case _: CustomTransientGroundType =>
          throw UnsupportedOperationException(s"Cannot map over $tp")
    end mapOver

    def mapOver(bounds: TypeBounds): TypeBounds =
      bounds match
        case bounds: TypeAlias =>
          derivedTypeAlias(bounds, atVariance(0)(this(bounds.alias)))
        case _ =>
          variance = -variance
          val low1 = this(bounds.low)
          variance = -variance
          val high1 = this(bounds.high)
          derivedTypeBounds(bounds, low1, high1)
    end mapOver

  abstract class NormalizingTypeMap(using Context) extends TypeMap:
    override protected def derivedSelect(tp: NamedType, pre: Type): Type =
      tp.normalizedDerivedSelect(pre)

    protected def mapArgs(args: List[TypeOrWildcard], tparams: List[TypeConstructorParam]): List[TypeOrWildcard] =
      args match
        case arg :: otherArgs if tparams.nonEmpty =>
          val arg1 = arg match
            case arg: WildcardTypeArg =>
              this(arg)
            case arg: Type =>
              /* `arg: TypeOrWildcard` allows the Type to be mapped to a WildcardTypeArg in this context.
               * Without it, we get a CCE that `WildcardTypeArg` cannot be cast to `Type`.
               */
              atVariance(variance * tparams.head.variance.sign)(this(arg: TypeOrWildcard))
          val otherArgs1 = mapArgs(otherArgs, tparams.tail)
          if (arg1 eq arg) && (otherArgs1 eq otherArgs) then args
          else arg1 :: otherArgs1
        case nil =>
          nil
    end mapArgs

    /** Map this function over given type */
    override def mapOver(tp: Type): Type =
      tp match
        case tp: AppliedType =>
          derivedAppliedType(tp, this(tp.tycon), mapArgs(tp.args, tp.tyconTypeParams))

        case _ =>
          super.mapOver(tp)
    end mapOver
  end NormalizingTypeMap
