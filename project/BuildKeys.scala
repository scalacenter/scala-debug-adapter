import sbt.Keys.{scalacOptions, version}
import sbt._
import sbtunidoc.BaseUnidocPlugin.autoImport.unidoc
import sbtunidoc.ScalaUnidocPlugin.autoImport.ScalaUnidoc

object BuildKeys {

  /**
   * Human readable project title.
   */
  lazy val projectTitle =
    settingKey[String]("Human readable project title (e.g. 'Cats Effect', 'Monix', etc)")

  /**
   * Project homepage root URL.
   *
   * Example: [[https://scalacenter.github.io/]]
   */
  lazy val projectWebsiteRootURL =
    settingKey[String]("Project homepage full URL")

  /**
   * Project homepage root URL.
   *
   * Example: `/my-library/` or `/`
   */
  lazy val projectWebsiteBasePath =
    settingKey[String]("Project homepage base path")

  /**
   * Full website URL.
   */
  lazy val projectWebsiteFullURL =
    Def.setting(s"${projectWebsiteRootURL.value.replaceAll("[/]+$", "")}/${projectWebsiteBasePath.value.replaceAll("^[/]+", "")}")

  /**
   * Example: alexandru, monix, typelevel, etc.
   */
  lazy val githubOwnerID =
    settingKey[String]("GitHub owner ID (e.g. user_id, organization_id)")

  /**
   * Example: alexandru, monix, typelevel, etc.
   */
  lazy val githubRelativeRepositoryID =
    settingKey[String]("GitHub repository ID (e.g. project_name)")

  /**
   * Example: `scalacenter/debug-scala-adapter`
   */
  lazy val githubFullRepositoryID =
    Def.setting(s"${githubOwnerID.value}/${githubOwnerID.value}")


  /**
   * Folder where the API docs will be uploaded when generating site.
   *
   * Typically: "api"
   */
  lazy val docsMappingsAPIDir =
    settingKey[String]("Name of subdirectory in site target directory for api docs")
}