package ch.epfl.scala.debugadapter.internal.stacktrace

import scala.util.Properties
import ch.epfl.scala.debugadapter.testfmk.FetchOptions
import coursier.maven.MavenRepository

class BinaryDecoderStats extends BinaryDecoderSuite:
  test("scala3-compiler:3.3.1"):
    val decoder = initDecoder("org.scala-lang", "scala3-compiler_3", "3.3.1")
    decoder.assertDecodeAll(
      expectedClasses = ExpectedCount(4426),
      expectedMethods = ExpectedCount(68421, ambiguous = 25, notFound = 33)
    )

  test("scala3-compiler:3.0.2"):
    val decoder = initDecoder("org.scala-lang", "scala3-compiler_3", "3.0.2")
    decoder.assertDecodeAll(
      expectedClasses = ExpectedCount(3859, notFound = 3),
      expectedMethods = ExpectedCount(60762, ambiguous = 24, notFound = 163)
    )

  test("io.github.vigoo:zio-aws-ec2_3:4.0.5 - slow".ignore):
    val decoder = initDecoder("io.github.vigoo", "zio-aws-ec2_3", "4.0.5")
    decoder.assertDecodeAll(
      ExpectedCount(8413, notFound = 9),
      ExpectedCount(157420, ambiguous = 6, notFound = 473)
    )

  test("org.tpolecat:doobie-h2_3:0.13.4"):
    val decoder = initDecoder("org.tpolecat", "doobie-h2_3", "0.13.4")
    decoder.assertDecodeAll(ExpectedCount(10), ExpectedCount(218))

  test("net.zygfryd:jackshaft_3:0.2.2".ignore):
    val decoder = initDecoder("net.zygfryd", "jackshaft_3", "0.2.2", FetchOptions(keepProvided = true))
    decoder.assertDecodeAll(ExpectedCount(49, notFound = 1), ExpectedCount(454, notFound = 1))

  test("company.jap:fields-core_3:0.4.16"):
    val decoder = initDecoder("company.jap", "fields-core_3", "0.4.16", FetchOptions(keepOptional = true))
    decoder.assertDecodeAll(ExpectedCount(245), ExpectedCount(2755, notFound = 92))

  test("org.clulab:processors-main_3:8.5.3"):
    val repository = MavenRepository("http://artifactory.cs.arizona.edu:8081/artifactory/sbt-release")
    val decoder = initDecoder("org.clulab", "processors-main_3", "8.5.3", FetchOptions(repositories = Seq(repository)))
    decoder.assertDecodeAll(
      ExpectedCount(798, throwables = 2),
      ExpectedCount(14708, ambiguous = 237, notFound = 148, throwables = 30)
    )

  test("com.github.simy4.xpath:xpath-to-xml-scala_3:2.3.7"):
    val decoder = initDecoder("com.github.simy4.xpath", "xpath-to-xml-scala_3", "2.3.7")
    decoder.assertDecodeAll(ExpectedCount(27), ExpectedCount(174, notFound = 2))

  test("com.zengularity:benji-google_3:2.2.1".ignore):
    val fetchOptions = FetchOptions(
      repositories = Seq(MavenRepository("https://oss.sonatype.org/content/repositories/snapshots")),
      keepProvided = true
    )
    val decoder = initDecoder("com.zengularity", "benji-google_3", "2.2.1", fetchOptions)
    decoder.assertDecodeAll(ExpectedCount(50, throwables = 10), ExpectedCount(450, throwables = 242))

  test("com.dimafeng:testcontainers-scala-scalatest-selenium_3:0.41.0".ignore):
    val fetchOptions = FetchOptions(keepProvided = true)
    val decoder = initDecoder("com.dimafeng", "testcontainers-scala-scalatest-selenium_3", "0.41.0", fetchOptions)
    decoder.assertDecodeAll(ExpectedCount(3), ExpectedCount(31, notFound = 3))

  test("com.zengularity:benji-vfs_3:2.2.1".ignore):
    val fetchOptions = FetchOptions(
      repositories = Seq(MavenRepository("https://oss.sonatype.org/content/repositories/snapshots")),
      keepProvided = true,
      keepOptional = true
    )
    val decoder = initDecoder("com.zengularity", "benji-vfs_3", "2.2.1", fetchOptions)
    decoder.assertDecodeAll(ExpectedCount(26), ExpectedCount(281, throwables = 13))

  test("dev.zio:zio-interop-cats_3:23.1.0.0"):
    val decoder = initDecoder("dev.zio", "zio-interop-cats_3", "23.1.0.0")
    decoder.assertDecodeAll(ExpectedCount(137, throwables = 21), ExpectedCount(2784, notFound = 2, throwables = 635))

  test("com.evolution:scache_3:5.1.2"):
    val fetchOptions = FetchOptions(
      repositories = Seq(MavenRepository("https://evolution.jfrog.io/artifactory/public"))
    )
    val decoder = initDecoder("com.evolution", "scache_3", "5.1.2", fetchOptions)
    decoder.assertDecodeAll(ExpectedCount(105), ExpectedCount(1509))

  test("com.github.j5ik2o:docker-controller-scala-dynamodb-local_:1.15.34"):
    val fetchOptions = FetchOptions(
      repositories = Seq(MavenRepository("https://maven.seasar.org/maven2/"))
    )
    val decoder = initDecoder("com.github.j5ik2o", "docker-controller-scala-dynamodb-local_3", "1.15.34", fetchOptions)
    decoder.assertDecodeAll(ExpectedCount(2), ExpectedCount(37))

  test("eu.ostrzyciel.jelly:jelly-grpc_3:0.5.3"):
    val fetchOptions = FetchOptions(exclusions = Seq("io.grpc" -> "grpc-core"))
    val decoder = initDecoder("eu.ostrzyciel.jelly", "jelly-grpc_3", "0.5.3", fetchOptions)
    decoder.assertDecodeAll(ExpectedCount(24), ExpectedCount(353))

  test("com.devsisters:zio-agones_3:0.1.0"):
    val fetchOptions = FetchOptions(exclusions = Seq("io.grpc" -> "grpc-core"))
    val decoder = initDecoder("com.devsisters", "zio-agones_3", "0.1.0", fetchOptions)
    decoder.assertDecodeAll(
      ExpectedCount(83, notFound = 21, throwables = 5),
      ExpectedCount(2784, throwables = 27)
    )

  test("org.log4s:log4s_3:1.10.0".ignore):
    val fetchOptions = FetchOptions(keepProvided = true)
    val decoder = initDecoder("org.log4s", "log4s_3", "1.10.0", fetchOptions)
    decoder.assertDecode("org.log4s.Warn", "java.lang.String name()", "")

  test("org.virtuslab.scala-cli:cli2_3:0.1.5".ignore):
    val fetchOptions = FetchOptions(keepProvided = true)
    val decoder = initDecoder("org.virtuslab.scala-cli", "cli2_3", "0.1.5", fetchOptions)
    decoder.assertDecodeAll()
