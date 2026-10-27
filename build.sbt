import IdeSettings.packagePrefix
import com.alecdorrington.omnibrowser.plugin.OmnibrowserPlugin
import sbt._
import sbt.Keys._
import sbtunidoc.BaseUnidocPlugin.autoImport.*
import sbtunidoc.ScalaUnidocPlugin

ThisBuild / scalaVersion := "3.9.0"
ThisBuild / organization := "com.alecdorrington"

scalacOptions ++= Seq(
  "-explain",
  "-explain-types",
  "-explain-cyclic",
)

// Omnibrowser adds only its own library, so the tests add its testkit.
// Laminar renders the popup, and Iris asks a large language model for the
// translations.
lazy val `codeswitch` = project
  .in(file("."))
  .enablePlugins(OmnibrowserPlugin, ScalaUnidocPlugin)
  .settings(
    packagePrefix := "com.alecdorrington.codeswitch",
    libraryDependencies ++= Seq(
      "com.alecdorrington" %% "iris"                % "0.2.0",
      "com.alecdorrington" %% "omnibrowser-laminar" % omnibrowserVersion.value,
      "com.alecdorrington" %% "omnibrowser-testkit" % omnibrowserVersion.value %
        Test,
      "org.typelevel" %% "munit-cats-effect" % "2.2.1" % Test,
    ),
    ScalaUnidoc / unidoc / scalacOptions ++= Seq("-project", "Codeswitch"),
  )
