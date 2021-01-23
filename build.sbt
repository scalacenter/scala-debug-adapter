import Boilerplate._
import BuildKeys._
import java.io.File
import sbt.url

//addCommandAlias("ci-run",     ";clean ;test:compile ;test")
//addCommandAlias("ci-package", ";scalafmtCheckAll ;package")
addCommandAlias("ci-doc",     ";unidoc ;site/makeMicrosite")
//addCommandAlias("ci",         ";project root ;reload ;+scalafmtCheckAll ;+ci-run ;+package ;ci-doc")
addCommandAlias("ci-doc-release", "; clean ;test:compile ;unidoc ;site/publishMicrosite")
//addCommandAlias("release",    ";+clean ;ci-release ;unidoc ;site/publishMicrosite")

inThisBuild(
  Seq(
    organization := "ch.epfl.scala",
    version := "1.0.0-SNAPSHOT",
    onLoadMessage := s"Welcome to scala-debug-adapter ${version.value}",
    scalaVersion := Dependencies.scala212,
    // resolvers ++= Seq(
    //   Resolver.bintrayRepo("scalacenter", "releases")
    // )
    // Microsite settings
    projectTitle := "Scala Debug Adapter",
    projectWebsiteRootURL := "https://scalacenter.github.io/",
    projectWebsiteBasePath := "/scala-debug-adapter/",
    githubOwnerID := "scalacenter",
    githubRelativeRepositoryID := "scala-debug-adapter",

    // ScalaDoc settings
    autoAPIMappings := true,
    scalacOptions ++= Seq(
      // Note, this is used by the doc-source-url feature to determine the
      // relative path of a given source file. If it's not a prefix of a the
      // absolute path of the source file, the absolute path of that file
      // will be put into the FILE_SOURCE variable, which is
      // definitely not what we want.
      "-sourcepath", file(".").getAbsolutePath.replaceAll("[.]$", "")
    ),

    // Project Info
    licenses := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
    homepage := Some(url(projectWebsiteFullURL.value)),
    scmInfo := Some(
      ScmInfo(
        url(s"https://github.com/${githubFullRepositoryID.value}"),
        s"scm:git@github.com:${githubFullRepositoryID.value}.git"
      )),

    developers := List(
      Developer(
        id="adpi2",
        name="Adrien Piquerez",
        email="adrien.piquerez@epfl.ch",
        url=url("https://github.com/adpi2")
      ),
      Developer(
        id="ericpeters",
        name="Eric Peters",
        email="eric@peters.org",
        url=url("https://github.com/er1c")
      ),
    ),
  )
)

lazy val root = project.in(file("."))
  .enablePlugins(ScalaUnidocPlugin)
  .aggregate(core)
  .settings(doNotPublishArtifact)
  .settings(unidocSettings)

lazy val core = project
  .enablePlugins(SbtJdiTools, BuildInfoPlugin, AutomateHeaderPlugin)
  .in(file("core"))
  .settings(
    name := "scala-debug-adapter",
    headerLicense := Some(HeaderLicense.ALv2("2020", "Scala Debug Adapter contributors")),
    libraryDependencies ++= List(
      Dependencies.asm,
      Dependencies.asmUtil,
      Dependencies.javaDebug,
      Dependencies.utest % Test,
      Dependencies.scalaCompiler % Test,
      Dependencies.io % Test
    ),
    testFrameworks += new TestFramework("utest.runner.Framework"),
    // Test / javaOptions += "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=1044",
    Test / fork := true,
    // build info is used to locate the library dependencies from the tests
    addBuildInfoToConfig(Test),
    buildInfoKeys := Seq[BuildInfoKey](
      BuildInfoKey.map(scalaInstance) { case (_, scalaInstance) => 
        "scalaLibraries" -> scalaInstance.libraryJars.mkString(File.pathSeparator)
      }
    )
  )

lazy val sbtPlugin = project
  .in(file("sbt-dap-plugin"))
  .enablePlugins(SbtPlugin)
  .settings(
    name := "sbt-dap-plugin"
  )
  .dependsOn(core)

lazy val bloopDap = project
  .in(file("bloop-dap"))
  .settings(
    name := "bloop-scala-debug-adapter",
    libraryDependencies ++= Seq(Dependencies.bloopFrontend)
  )
  .dependsOn(core)

lazy val site = project.in(file("site"))
  //.disablePlugins(MimaPlugin)
  .enablePlugins(MicrositesPlugin)
  .enablePlugins(MdocPlugin)
  .settings(doNotPublishArtifact)
  .dependsOn(core)
  .settings {
    import microsites._
    Seq(
      micrositeName := projectTitle.value,
      micrositeDescription := "Scala Debug Adapter",
      micrositeOrganizationHomepage := "https://scala.epfl.ch/",
      micrositeTwitterCreator := "@scala_lang",
      micrositeGithubOwner := githubOwnerID.value,
      micrositeGithubRepo := githubRelativeRepositoryID.value,
      micrositeUrl := projectWebsiteRootURL.value.replaceAll("[/]+$", ""),
      micrositeBaseUrl := projectWebsiteBasePath.value.replaceAll("[/]+$", ""),
      micrositeDocumentationUrl := s"${projectWebsiteFullURL.value.replaceAll("[/]+$", "")}/${docsMappingsAPIDir.value}/",
      micrositeGitterChannelUrl := githubFullRepositoryID.value,
      micrositeFooterText := None,
      micrositeHighlightTheme := "atom-one-light",
      micrositePalette := Map(
        "brand-primary" -> "#3e5b95",
        "brand-secondary" -> "#294066",
        "brand-tertiary" -> "#2d5799",
        "gray-dark" -> "#49494B",
        "gray" -> "#7B7B7E",
        "gray-light" -> "#E5E5E6",
        "gray-lighter" -> "#F4F3F4",
        "white-color" -> "#FFFFFF"
      ),
      micrositeCompilingDocsTool := WithMdoc,
      fork in mdoc := true,
      //scalacOptions.in(Tut) ~= filterConsoleScalacOptions,
      libraryDependencies += "com.47deg" %% "github4s" % Dependencies.gitHub4sVersion,
      micrositePushSiteWith := GitHub4s,
      micrositeGithubToken := sys.env.get("GITHUB_TOKEN"),
      micrositeExtraMdFilesOutput := (resourceManaged in Compile).value / "jekyll",
      micrositeConfigYaml := ConfigYml(
        yamlPath = Some((resourceDirectory in Compile).value / "microsite" / "_config.yml")
      ),
      micrositeExtraMdFiles := Map(
        file("README.md") -> ExtraMdFileConfig("index.md", "page", Map("title" -> "Home", "section" -> "home", "position" -> "100")),
        file("CHANGELOG.md") -> ExtraMdFileConfig("CHANGELOG.md", "page", Map("title" -> "Change Log", "section" -> "changelog", "position" -> "101")),
        file("CONTRIBUTING.md") -> ExtraMdFileConfig("CONTRIBUTING.md", "page", Map("title" -> "Contributing", "section" -> "contributing", "position" -> "102")),
        file("CODE_OF_CONDUCT.md") -> ExtraMdFileConfig("CODE_OF_CONDUCT.md", "page", Map("title" -> "Code of Conduct", "section" -> "code of conduct", "position" -> "103")),
        file("LICENSE.md") -> ExtraMdFileConfig("LICENSE.md", "page", Map("title" -> "License", "section" -> "license", "position" -> "104")),
      ),
      docsMappingsAPIDir := s"api",
      addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc) in root, docsMappingsAPIDir),
      sourceDirectory in Compile := baseDirectory.value / "src",
      sourceDirectory in Test := baseDirectory.value / "test",
      mdocIn := (sourceDirectory in Compile).value / "mdoc",

      run in Compile := {
        import scala.sys.process._

        val s: TaskStreams = streams.value
        val shell: Seq[String] = if (sys.props("os.name").contains("Windows")) Seq("cmd", "/c") else Seq("bash", "-c")

        val jekyllServe: String = s"jekyll serve --open-url --baseurl ${(micrositeBaseUrl in Compile).value}"

        s.log.info("Running Jekyll...")
        Process(shell :+ jekyllServe, (micrositeExtraMdFilesOutput in Compile).value) !
      },
    )
  }