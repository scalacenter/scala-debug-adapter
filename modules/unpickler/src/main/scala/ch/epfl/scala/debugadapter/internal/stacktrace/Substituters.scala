// package stacktrace

// import tastyquery.Contexts.*
// import tastyquery.Symbols.*
// import tastyquery.Types.*
// import tastyquery.TypeMaps.*

// object Substituters:

//   // def substBinders(tp: TypeMappable, from: Binders, to: Binders): tp.ThisTypeMappableType =
//   //   new SubstBindingMap(from, to).apply(tp)

//   // def substParams(tp: TypeMappable, from: Binders, to: List[TypeOrWildcard])(using Context): tp.ThisTypeMappableType =
//   //   new SubstParamsMap(from, to).apply(tp)

//   // def substRecThis(tp: TypeMappable, from: RecType, to: Type)(using Context): tp.ThisTypeMappableType =
//   //   new SubstRecThisMap(from, to).apply(tp)

//   def substClassTypeParams(tp: TypeMappable, from: List[ClassTypeParamSymbol], to: List[TypeOrWildcard])(
//     using Context
//   ): tp.ThisTypeMappableType =
//     new SubstClassTypeParamsMap(from, to).apply(tp)

//   def substLocalParams(tp: TypeMappable, from: List[Symbol], to: List[Type]): tp.ThisTypeMappableType =
//     if from.isEmpty then tp
//     else new SubstLocalParamsMap(from, to).apply(tp)

//   def substLocalThisClassTypeParams(
//     tp: TypeMappable,
//     from: List[ClassTypeParamSymbol],
//     to: List[Type]
//   ): tp.ThisTypeMappableType =
//     if from.isEmpty then tp
//     else new SubstLocalThisClassTypeParamsMap(from, to).apply(tp)

//   def substRefinementThis(tp: TypeMappable, from: ClassSymbol, to: RecThis): tp.ThisTypeMappableType =
//     new SubstRefinementThisMap(from, to).apply(tp)

//   // final class SubstBindingMap(from: Binders, to: Binders) extends TypeMap:
//   //   protected def transform(tp: TypeMappable): TypeMappable =
//   //     tp match
//   //       case tp: BoundType =>
//   //         if tp.binders eq from then tp.copyBoundType(to.asInstanceOf[tp.BindersType]) else tp
//   //       case tp: NamedType =>
//   //         tp.prefix match
//   //           case NoPrefix | _: PackageRef => tp
//   //           case prefix: Type             => tp.derivedSelect(apply(prefix))
//   //       case _: ThisType =>
//   //         tp
//   //       case tp: AppliedType =>
//   //         tp.map(apply(_), apply(_))
//   //       case _ =>
//   //         mapOver(tp)
//   //   end transform
//   // end SubstBindingMap

//   // private final class SubstParamsMap(from: Binders, to: List[TypeOrWildcard])(using Context) extends NormalizingTypeMap:
//   //   protected def transform(tp: TypeMappable): TypeMappable =
//   //     tp match
//   //       case tp: ParamRef =>
//   //         if tp.binders eq from then to(tp.paramNum) else tp
//   //       case tp: NamedType =>
//   //         tp.prefix match
//   //           case NoPrefix | _: PackageRef => tp
//   //           case prefix: Type             => tp.normalizedDerivedSelect(apply(prefix))
//   //       case _: ThisType =>
//   //         tp
//   //       case tp: AppliedType =>
//   //         tp.map(apply(_), apply(_))
//   //       case _ =>
//   //         mapOver(tp)
//   //   end transform
//   // end SubstParamsMap

//   // private final class SubstRecThisMap(from: RecType, to: Type)(using Context) extends NormalizingTypeMap:
//   //   protected def transform(tp: TypeMappable): TypeMappable =
//   //     tp match
//   //       case tp: RecThis =>
//   //         if tp.binders eq from then to else tp
//   //       case tp: NamedType =>
//   //         tp.prefix match
//   //           case NoPrefix | _: PackageRef => tp
//   //           case prefix: Type             => tp.normalizedDerivedSelect(apply(prefix))
//   //       case _: ThisType =>
//   //         tp
//   //       case tp: AppliedType =>
//   //         tp.map(apply(_), apply(_))
//   //       case _ =>
//   //         mapOver(tp)
//   //   end transform
//   // end SubstRecThisMap

//   private final class SubstClassTypeParamsMap(from: List[ClassTypeParamSymbol], to: List[TypeOrWildcard])(using Context)
//       extends NormalizingTypeMap:
//     protected def transform(tp: TypeMappable): TypeMappable =
//       tp match
//         case tp: NamedType =>
//           tp.prefix match
//             case _: ThisType if tp.isSomeClassTypeParamRef =>
//               var fs = from
//               var ts = to
//               while fs.nonEmpty && ts.nonEmpty do
//                 if tp.isClassTypeParamRef(fs.head) then return ts.head
//                 fs = fs.tail
//                 ts = ts.tail
//               tp
//             case prefix: Type =>
//               tp.normalizedDerivedSelect(apply(prefix))
//             case NoPrefix | _: PackageRef =>
//               tp
//         case _: ThisType | _: BoundType =>
//           tp
//         case _ =>
//           mapOver(tp)
//     end transform
//   end SubstClassTypeParamsMap

//   private final class SubstLocalParamsMap(from: List[Symbol], to: List[Type]) extends TypeMap:
//     protected def transform(tp: TypeMappable): TypeMappable =
//       tp match
//         case tp: NamedType =>
//           tp.prefix match
//             case NoPrefix =>
//               var fs = from
//               var ts = to
//               while fs.nonEmpty && ts.nonEmpty do
//                 if tp.isLocalRef(fs.head) then return ts.head
//                 fs = fs.tail
//                 ts = ts.tail
//               tp
//             case prefix: PackageRef =>
//               tp
//             case prefix: Type =>
//               tp.derivedSelect(this(prefix))
//         case _: ThisType | _: BoundType =>
//           tp
//         case _ =>
//           mapOver(tp)
//     end transform
//   end SubstLocalParamsMap

//   private final class SubstLocalThisClassTypeParamsMap(from: List[ClassTypeParamSymbol], to: List[Type])
//       extends TypeMap:
//     protected def transform(tp: TypeMappable): TypeMappable =
//       tp match
//         case tp: NamedType =>
//           tp.prefix match
//             case NoPrefix | _: PackageRef =>
//               tp
//             case prefix: ThisType if tp.isSomeClassTypeParamRef =>
//               /* In theory, we should check that `prefix.cls` is indeed the
//                * owner of the class type params we want to substitute.
//                * However, we are not allowed to do that, since that would
//                * require resolving arbitrary `TypeRef`s. Fortunately, by
//                * construction, we know that for any `C.this.T` where `T` is a
//                * class type parameter, `C` is necessarily the owner of `T`.
//                * Therefore, we can get away without looking at `prefix.cls`.
//                */
//               var fs = from
//               var ts = to
//               while fs.nonEmpty && ts.nonEmpty do
//                 if tp.isClassTypeParamRef(fs.head) then return ts.head
//                 fs = fs.tail
//                 ts = ts.tail
//               tp.derivedSelect(this(prefix))
//             case prefix: Type =>
//               tp.derivedSelect(this(prefix))
//         case _: ThisType | _: BoundType =>
//           tp
//         case _ =>
//           mapOver(tp)
//     end transform
//   end SubstLocalThisClassTypeParamsMap

//   private final class SubstRefinementThisMap(from: ClassSymbol, to: RecThis) extends TypeMap:
//     protected def transform(tp: TypeMappable): TypeMappable =
//       tp match
//         case tp: ThisType =>
//           if tp.tref.isLocalRef(from) then to else tp
//         case _: BoundType =>
//           tp
//         case _ =>
//           mapOver(tp)
//     end transform
//   end SubstRefinementThisMap

// end Substituters
