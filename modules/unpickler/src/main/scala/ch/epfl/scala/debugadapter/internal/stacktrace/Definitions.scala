package ch.epfl.scala.debugadapter.internal.stacktrace

import tastyquery.Contexts.Context
import tastyquery.Names.*

class Definitions(using ctx: Context) {
  val scalaPackage = ctx.defn.scalaPackage
  val scalaRuntimePackage =  scalaPackage.getPackageDecl(SimpleName("runtime")).get
  val javaPackage = ctx.defn.RootPackage.getPackageDecl(SimpleName("java")).get 
  val javaIOPackage= javaPackage.getPackageDecl(SimpleName("io")).get
  val partialFunction = scalaPackage.getDecl(typeName("PartialFunction")).get.asClass
  val abstractPartialFunction = scalaRuntimePackage.getDecl(typeName("AbstractPartialFunction")).get.asClass
  val serializable = javaIOPackage.getDecl(typeName("Serializable")).get.asClass
}

