package ch.epfl.scala.debugadapter.internal

import utest._
import coursier._
import coursier.params.ResolutionParams
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit

import ch.epfl.scala.debugadapter.Coursier
/** This is not a test class.
 *  This class prints some stats about loading the lookup of some libraries
*/
object ClassPathEntryLookUpStats extends TestSuite {
  def tests = Tests {
    "scala-lang" - {
      val org = "org.scala-lang"
      printStats(org, "scala-library", "2.13.6")
      printStats(org, "scala-compiler", "2.13.6")
      printStats(org, "scala3-library_3", "3.0.0")
      printStats(org, "scala3-compiler_3", "3.0.0")
    }

    "scalatest" - {
      val org = "org.scalatest"
      val version = "3.2.9"
      printStats(org, "scalatest-core_3", version)
      // printStats("org.scalactic", "scalactic_3", version)
      // printStats(org, "scalatest-matchers-core_3", version)
    }

    "typelevel" - {
      val org = "org.typelevel"
      printStats(org, "cats-core_3", "2.6.1")
      printStats(org, "cats-kernel_3", "2.6.1")
      
      // val catsEffectVersion = "3.1.1"
      // printStats(org, "cats-effect-kernel_3", catsEffectVersion)
      // printStats(org, "cats-effect-std_3", catsEffectVersion)
      // printStats(org, "cats-effect_3", catsEffectVersion)
      
      val shapelessOrg = "com.chuusai"
      printStats(shapelessOrg, "shapeless_2.13", "2.3.7")
      // printStats(org, "shapeless3-deriving_3", "3.0.1")
      // printStats(org, "shapeless3-typeable_3", "3.0.1")

      // val fs2Org = "co.fs2"
      // val fs2Version = "3.0.6"
      // printStats(fs2Org, "fs2-core_3", fs2Version)
      // printStats(fs2Org, "fs2-io_3", fs2Version)
      // printStats(fs2Org, "fs2-reactive-streams_3", fs2Version)

      // val doobieOrg = "org.tpolecat"
      // val doobieVersion = "0.13.4"
      // printStats(doobieOrg, "doobie-core_3", doobieVersion)
      // printStats(doobieOrg, "doobie-h2_3", doobieVersion)
      // printStats(doobieOrg, "doobie-postgres_3", doobieVersion)

      //printStats("org.scalacheck", "scalacheck_3", "1.15.4")

      // val monixOrg = "io.monix"
      // val monixVersion = "3.4.0"
      // printStats(monixOrg, "monix-catnap_3", monixVersion)
      // printStats(monixOrg, "monix-eval_3", monixVersion)
      // printStats(monixOrg, "monix-execution_3", monixVersion)
      // printStats(monixOrg, "monix-java_3", monixVersion)
      // printStats(monixOrg, "monix-reactive_3", monixVersion)
      // printStats(monixOrg, "monix-tail_3", monixVersion)
    }

    "sbt" - {
      val org = "org.scala-sbt"
      val version = "1.5.4"
      printStats(org, "main_2.12", version)
      printStats("io.get-coursier", "lm-coursier-shaded_2.12", "2.0.8") // shaded jars create lot of orphan class files
      // printStats(org, "io_2.12", "1.5.1")
      // printStats(org, "librarymanagement-core_2.12", "1.5.2")
      // printStats(org, "librarymanagement-ivy_2.12", "1.5.2")
      // printStats(org, "main-settings_2.12", version)
      // printStats(org, "run_2.12", version)
      // printStats(org, "scripted-plugin_2.12", version)
      // printStats(org, "zinc-compile_2.12", "1.5.5")
      // printStats(org, "zinc-lm-integration_2.12", version)
    }

    "spark" - {
      val org = "org.apache.spark"
      val version = "3.1.2" 
      printStats(org, "spark-core_2.12", version)
      // printStats(org, "spark-avro_2.12", version)
      printStats(org, "spark-catalyst_2.12", version)
      // printStats(org, "spark-streaming_2.12", version)
      printStats(org, "spark-sql_2.12", version)
      printStats(org, "spark-mllib_2.12", version)
    }

    "akka" - {
      val org = "com.typesafe.akka"
      val version = "2.6.14"
      printStats(org, "akka-actor_2.13", version)
      // printStats(org, "akka-cluster_2.13", version)
      printStats(org, "akka-stream_2.13", version)
      // printStats(org, "akka-persistence_2.13", version)

      val httpVersion = "10.2.4"
      printStats(org, "akka-http-core_2.13", httpVersion)
      // printStats(org, "akka-parsing_2.13", httpVersion)
      // printStats(org, "akka-http_2.13", httpVersion)
    }

    "zio" - {
      val org = "dev.zio"
      val version = "1.0.9"
      printStats(org, "zio_3", version)
      printStats(org, "zio-streams_3", version)
    }

    "guava" - printStats("com.google.guava", "guava", "30.1.1-jre")

    "play" - {
      val org = "com.typesafe.play"
      val version = "2.8.8"
      printStats(org, "play_2.13", version)
      // printStats(org, "play-streams_2.13", version)
      // printStats(org, "play-java_2.13", version)
      // printStats(org, "play-cache_2.13", version)
      // printStats(org, "play-jdbc_2.13", version)
    }

    "scalaz" - {
      val org = "org.scalaz"
      val version = "7.3.3"
      printStats(org, "scalaz-core_2.13", version)
      // printStats(org, "scalaz-effect_2.13", version)
    }

    "scalameta" - {
      val org = "org.scalameta"
      printStats(org, "semanticdb-scalac_2.13.6", "4.4.23") // this one is empty because the source jar is empty
      printStats(org, "semanticdb-scalac-core_2.13.6", "4.4.23")
    }
  }

  private def printStats(org: String, name: String, version: String): Unit = {
    val classPathEntry = Coursier.fetchOnly(org, name, version)
    val (duration, lookup) = Stats.timed(ClassPathEntryLookUp(classPathEntry))
    val classFileCount = lookup.fullyQualifiedNames.size
    val orphanClassFileCount = lookup.orphanClassFiles.size
    println(s"${classPathEntry.name}:")
    println(s"  - $classFileCount classes loaded in $duration")
    if (orphanClassFileCount > 0) {
      val orphanClassFilePercent = orphanClassFileCount * 100 / classFileCount
      println(s"  - $orphanClassFileCount orphan class files ($orphanClassFilePercent%)")
    }
  }
}
