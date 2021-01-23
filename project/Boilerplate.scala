import BuildKeys.{githubFullRepositoryID, projectTitle}
import sbt.Keys.{publish, publishArtifact, publishLocal, publishTo, scalacOptions, skip, version}
import sbt.{Opts, file}
import sbtunidoc.BaseUnidocPlugin.autoImport.unidoc
import sbtunidoc.ScalaUnidocPlugin.autoImport.ScalaUnidoc

object Boilerplate {

  /**
   * Skip publishing artifact for this project.
   */
  lazy val doNotPublishArtifact = Seq(
    skip in publish := true,
    publish := {},
    publishLocal := {},
    publishArtifact := false,
    publishTo := None
  )

  /**
   * Configures generated API documentation website.
   */
  lazy val unidocSettings = Seq(
    scalacOptions in (ScalaUnidoc, unidoc) +=
      "-Xfatal-warnings",
    scalacOptions in (ScalaUnidoc, unidoc) --=
      Seq("-Ywarn-unused-import", "-Ywarn-unused:imports"),
    scalacOptions in (ScalaUnidoc, unidoc) ++=
      Opts.doc.title(projectTitle.value),
    scalacOptions in (ScalaUnidoc, unidoc) ++=
      Opts.doc.sourceUrl(s"https://github.com/${githubFullRepositoryID.value}/tree/v${version.value}€{FILE_PATH}.scala"),
    scalacOptions in (ScalaUnidoc, unidoc) ++=
      Seq("-doc-root-content", file("rootdoc.txt").getAbsolutePath),
    scalacOptions in (ScalaUnidoc, unidoc) ++=
      Opts.doc.version(version.value)
  )
}