package ch.epfl.scala.debugadapter.internal.stacktrace

import tastyquery.Contexts.Context
import tastyquery.Names.*
import tastyquery.Types.*

class Definitions(using ctx: Context):
  export ctx.defn.*

  val scalaRuntimePackage = scalaPackage.getPackageDecl(SimpleName("runtime")).get
  val javaPackage = RootPackage.getPackageDecl(SimpleName("java")).get
  val javaIoPackage = javaPackage.getPackageDecl(SimpleName("io")).get
  val javaLangInvokePackage = javaLangPackage.getPackageDecl(SimpleName("invoke")).get

  val PartialFunctionClass = scalaPackage.getDecl(typeName("PartialFunction")).get.asClass
  val AbstractPartialFunctionClass = scalaRuntimePackage.getDecl(typeName("AbstractPartialFunction")).get.asClass
  val SerializableClass = javaIoPackage.getDecl(typeName("Serializable")).get.asClass

  val SerializedLambdaType: Type = TypeRef(javaLangInvokePackage.packageRef, typeName("SerializedLambda"))
  val DeserializeLambdaType = MethodType(List(SimpleName("arg0")), List(SerializedLambdaType), ObjectType)

  val Function0Type = TypeRef(scalaPackage.packageRef, typeName("Function0"))
