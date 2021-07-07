package ch.epfl.scala.debugadapter.internal

import utest._
import coursier._
import ch.epfl.scala.debugadapter.Coursier
import coursier.Dependency
import ch.epfl.scala.debugadapter.NoopLogger

object SourceLookUpProviderStats extends TestSuite {
  def tests = Tests {
    "scaladex" - printStats("scaladex (Test)")(
      dep"ch.qos.logback:logback-classic:1.1.7",
      dep"com.typesafe.scala-logging:scala-logging_2.13:3.9.2",
      dep"com.getsentry.raven:raven-logback:8.0.3",
      dep"org.scalatest:scalatest_2.13:3.0.9",
      dep"com.github.nscala-time:nscala-time_2.13:2.24.0",
      dep"com.typesafe.akka:akka-http-core_2.13:10.1.12",
      dep"com.sksamuel.elastic4s:elastic4s-client-esjava_2.13:7.10.2",
      dep"org.json4s:json4s-native_2.13:3.6.9",
      dep"org.typelevel:jawn-json4s_2.13:1.0.0",
      dep"com.typesafe.play:play-json_2.13:2.9.0",
      dep"com.typesafe.akka:akka-testkit_2.13:2.6.5",
      dep"com.typesafe.akka:akka-slf4j_2.13:2.6.5",
      dep"ch.megard:akka-http-cors_2.13:0.4.3",
      dep"com.softwaremill.akka-http-session:core_2.13:0.5.11",
      dep"com.typesafe.akka:akka-http_2.13:10.1.12",
      dep"org.webjars.bower:bootstrap-sass:3.3.6",
      dep"org.webjars.bower:bootstrap-switch:3.3.2",
      dep"org.webjars.bower:bootstrap-select:1.10.0",
      dep"org.webjars.bower:font-awesome:4.6.3",
      dep"org.webjars.bower:jquery:2.2.4",
      dep"org.webjars.bower:raven-js:3.11.0",
      dep"org.webjars.bower:select2:4.0.3",
      dep"org.apache.logging.log4j:log4j-core:2.13.3",
      dep"com.lihaoyi:fastparse_2.13:2.3.0",
      dep"org.scala-lang.modules:scala-parallel-collections_2.13:0.2.0",
      dep"com.typesafe.akka:akka-stream_2.13:2.6.5",
      dep"me.tongfei:progressbar:0.5.5",
      dep"org.apache.maven:maven-model-builder:3.3.9",
      dep"org.jsoup:jsoup:1.10.1",
      dep"com.typesafe.play:play-ahc-ws_2.13:2.8.2",
      dep"org.apache.ivy:ivy:2.4.0",
      dep"de.heikoseeberger:akka-http-json4s_2.13:1.29.1"
    )
  }

  private def printStats(project: String)(deps: Dependency*): Unit = {
    val classPath = Coursier.fetch(deps)
    val (duration, lookup) = Stats.timed(SourceLookUpProvider(classPath, NoopLogger))
    println(s"$project:")
    println(s"  - loaded in $duration")
  }
}
