package ch.epfl.scala.debugadapter.internal

import utest._
import coursier._
import coursier.jvm.JvmCache
import coursier.params.ResolutionParams
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit

import ch.epfl.scala.debugadapter.Coursier
import ch.epfl.scala.debugadapter.ClassPathEntry
import scala.util.Properties
import ch.epfl.scala.debugadapter.JavaRuntime
import ch.epfl.scala.debugadapter.ClassEntry

/**
 * This is a test class that also
 * prints some stats about loading the look-up of some libraries
 */
object ClassEntryLookUpStats extends TestSuite {
  private val jvmCache = JvmCache().withDefaultIndex

  def tests = Tests {
    "adopt:1.8.0-292" - {
      printAndCheck("adopt:1.8.0-292")(
        classCount => classCount > 0,
        orphanClassCount => orphanClassCount == 0
      )
    }

    "adopt:1.9.0-0" - {
      printAndCheck("adopt:1.9.0-0")(
        classCount => classCount > 0,
        orphanClassCount => orphanClassCount == 0
      )
    }

    "adopt:1.10.0-2" - {
      printAndCheck("adopt:1.10.0-2")(
        classCount => classCount > 0,
        orphanClassCount => orphanClassCount == 0
      )
    }

    "adopt:1.11.0-11" - {
      printAndCheck("adopt:1.11.0-11")(
        classCount => classCount > 0,
        orphanClassCount => orphanClassCount == 0
      )
    }

    "adopt:1.12.0-2" - {
      printAndCheck("adopt:1.12.0-2")(
        classCount => classCount > 0,
        orphanClassCount => orphanClassCount == 0
      )
    }

    "adopt:1.13.0-2" - {
      printAndCheck("adopt:1.13.0-2")(
        classCount => classCount > 0,
        orphanClassCount => orphanClassCount == 0
      )
    }

    "adopt:1.14.0-2" - {
      printAndCheck("adopt:1.14.0-2")(
        classCount => classCount > 0,
        orphanClassCount => orphanClassCount == 0
      )
    }

    "adopt:1.15.0-2" - {
      printAndCheck("adopt:1.15.0-2")(
        classCount => classCount > 0,
        orphanClassCount => orphanClassCount == 0
      )
    }

    "adopt:1.16.0-1" - {
      printAndCheck("adopt:1.16.0-1")(
        classCount => classCount > 0,
        orphanClassCount => orphanClassCount == 0
      )
    }

    "zulu:1.17.0-0" - {
      printAndCheck("zulu:1.17.0-0")(
        classCount => classCount > 0,
        orphanClassCount => orphanClassCount == 0
      )
    }

    "scala-lang" - {
      val org = "org.scala-lang"
      printAndCheck(org, "scala-library", "2.13.7")(2870, 0)
      printAndCheck(org, "scala-compiler", "2.13.7")(3399, 3)
      printAndCheck(org, "scala3-library_3", "3.0.0")(487, 0)
      printAndCheck(org, "scala3-compiler_3", "3.0.0")(3565, 0)
    }

    "scalatest" - {
      val org = "org.scalatest"
      val version = "3.2.9"
      printAndCheck(org, "scalatest-core_3", version)(1282, 0)
    }

    "typelevel" - {
      val org = "org.typelevel"
      printAndCheck(org, "cats-core_3", "2.6.1")(2330, 0)
      printAndCheck(org, "cats-kernel_3", "2.6.1")(1020, 0)

      val shapelessOrg = "com.chuusai"
      printAndCheck(shapelessOrg, "shapeless_2.13", "2.3.7")(2228, 0)
    }

    "sbt" - {
      val org = "org.scala-sbt"
      val version = "1.5.4"
      printAndCheck(org, "main_2.12", version)(911, 0)
      printAndCheck("io.get-coursier", "lm-coursier-shaded_2.12", "2.0.8")(
        3972,
        0
      )
    }

    "spark" - {
      val org = "org.apache.spark"
      val version = "3.1.2"
      printAndCheck(org, "spark-core_2.12", version)(3764, 1)
      printAndCheck(org, "spark-catalyst_2.12", version)(3654, 0)
      printAndCheck(org, "spark-sql_2.12", version)(2306, 0)
      printAndCheck(org, "spark-mllib_2.12", version)(2302, 2)
    }

    "akka" - {
      val org = "com.typesafe.akka"
      val version = "2.6.14"
      printAndCheck(org, "akka-actor_2.13", version)(1793, 0)
      printAndCheck(org, "akka-stream_2.13", version)(2535, 0)

      val httpVersion = "10.2.4"
      printAndCheck(org, "akka-http-core_2.13", httpVersion)(2084, 0)
    }

    "zio" - {
      val org = "dev.zio"
      val version = "1.0.9"
      printAndCheck(org, "zio_3", version)(810, 0)
      printAndCheck(org, "zio-streams_3", version)(183, 0)
    }

    "guava" - printAndCheck("com.google.guava", "guava", "30.1.1-jre")(2030, 0)

    "play" - {
      val org = "com.typesafe.play"
      val version = "2.8.8"
      printAndCheck(org, "play_2.13", version)(1383, 2)
    }

    "scalaz" - {
      val org = "org.scalaz"
      val version = "7.3.3"
      printAndCheck(org, "scalaz-core_2.13", version)(3507, 0)
    }

    "scalameta" - {
      val org = "org.scalameta"
      printAndCheck(org, "semanticdb-scalac-core_2.13.6", "4.4.23")(113, 0)
    }
  }

  private def printAndCheck(jvm: String)(
      classCountAssertion: Int => Boolean,
      orphanAssertion: Int => Boolean
  ): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val javaHome = jvmCache.get(jvm).unsafeRun()
    val javaRuntime = JavaRuntime(javaHome.toPath).get
    printAndCheck(jvm, javaRuntime)(classCountAssertion, orphanAssertion)
  }

  private def printAndCheck(org: String, name: String, version: String)(
      expectedClasses: Int,
      expectedOrphans: Int
  ): Unit = {
    val entry = Coursier.fetchOnly(org, name, version)
    printAndCheck(name, entry)(_ == expectedClasses, _ == expectedOrphans)
  }

  private def printAndCheck(name: String, entry: ClassEntry)(
      classCountAssertion: Int => Boolean,
      orphanAssertion: Int => Boolean
  ): Unit = {
    val (duration, lookup) = Stats.timed(ClassEntryLookUp(entry))
    val classCount = lookup.fullyQualifiedNames.size
    val orphanClassCount = lookup.orphanClassFiles.size
    println(s"$name:")
    println(s"  - $classCount classes loaded in $duration")
    if (orphanClassCount > 0) {
      val orphanClassFilePercent =
        (orphanClassCount * 10000 / classCount).toFloat / 100
      println(
        s"  - $orphanClassCount orphan class files ($orphanClassFilePercent%)"
      )
    }
    assert(classCountAssertion(classCount), orphanAssertion(orphanClassCount))
  }
}
