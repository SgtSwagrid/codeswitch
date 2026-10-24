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

// Omnibrowser adds its own library, and its testkit for the tests, itself.
lazy val `scala-browser-plugin-template` = project
  .in(file("."))
  .enablePlugins(OmnibrowserPlugin, ScalaUnidocPlugin)
  .settings(
    packagePrefix := "com.alecdorrington",
    libraryDependencies +=
      "org.typelevel" %% "munit-cats-effect" % "2.2.1" % Test,
    ScalaUnidoc / unidoc / scalacOptions ++= Seq(
      "-project",
      "Scala Browser Plugin Template",
    ),
  )
