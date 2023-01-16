ThisBuild / organization := "com.innovenso"
ThisBuild / organizationName := "Innovenso"
ThisBuild / organizationHomepage := Some(url("https://innovenso.com"))
ThisBuild / scalaVersion := "2.13.10"
ThisBuild / resolvers += Resolver.mavenLocal
ThisBuild / resolvers += "Config Maven Repo" at "https://maven.pkg.github.com/genius-fish/config"
ThisBuild / resolvers += "Logging Maven Repo" at "https://maven.pkg.github.com/genius-fish/logging"
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/innozu/model"),
    "scm:git@github.com:innozu/model.git"
  )
)
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

ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
ThisBuild / publishTo := sonatypePublishToBundle.value
ThisBuild / credentials += Credentials(
  realm = "Sonatype Nexus Repository Manager",
  host = "s01.oss.sonatype.org",
  userName = sys.env.getOrElse("INNOVENSO_PUBLISH_OSSRH_USERNAME", "none"),
  passwd = sys.env.getOrElse("INNOVENSO_PUBLISH_OSSRH_PASSWORD", "none")
)

usePgpKeyHex(sys.env.getOrElse("INNOVENSO_PUBLISH_SIGNING_KEY_ID", "none"))

lazy val root = project
  .in(file("."))
  .settings(
    name := "innozu-model",
    libraryDependencies ++= Dependencies.Testing.*,
    libraryDependencies ++= Dependencies.Lorem.*,
    libraryDependencies ++= Dependencies.GeniusFish.*,
    libraryDependencies ++= Dependencies.Apache.Commons.*
  )

addCommandAlias("deploy", "publishSigned;sonatypeBundleRelease;publishM2")
