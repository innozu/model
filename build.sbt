ThisBuild / organization := "com.innovenso"
ThisBuild / organizationName := "Innovenso"
ThisBuild / organizationHomepage := Some(url("https://innovenso.com"))
ThisBuild / scalaVersion := "2.13.10"
ThisBuild / resolvers += Resolver.mavenLocal
ThisBuild / resolvers += "Config Maven Repo" at "https://maven.pkg.github.com/genius-fish/config"
ThisBuild / resolvers += "Logging Maven Repo" at "https://maven.pkg.github.com/genius-fish/logging"
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / developers := List(
  Developer(
    id = "jlust",
    name = "Jurgen Lust",
    email = "jurgen@innovenso.com",
    url = url("https://innovenso.com")
  )
)
ThisBuild / description := "The Innovenso Townplanner is a set of libraries used to document a company's enterprise architecture."
ThisBuild / licenses := List(
  "GNU General Public License v3" -> new URL(
    "https://www.gnu.org/licenses/gpl-3.0.txt"
  )
)
ThisBuild / homepage := Some(url("https://townplanner.be"))

ThisBuild / publishTo := Some(
  "Maven Repo" at "https://maven.pkg.github.com/innozu/model"
)
ThisBuild / publishMavenStyle := true
ThisBuild / credentials += Credentials(
  "GitHub Package Registry",
  "maven.pkg.github.com",
  sys.env.getOrElse("GITHUB_PACKAGES_OWNER", "none"),
  sys.env.getOrElse("GITHUB_PACKAGES_TOKEN", "none")
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "innozu-model",
    libraryDependencies ++= Dependencies.Testing.*,
    libraryDependencies ++= Dependencies.Lorem.*,
    libraryDependencies ++= Dependencies.GeniusFish.*,
    libraryDependencies ++= Dependencies.Apache.Commons.*
  )

addCommandAlias("deploy", "publishLocal;publish")
