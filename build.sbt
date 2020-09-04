Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / githubOwner := "MakeNowJust-Labo"
ThisBuild / githubRepository := "gen"

ThisBuild / scalaVersion := "2.13.3"
ThisBuild / scalacOptions ++= Seq(
  "-encoding",
  "UTF-8",
  "-feature",
  "-deprecation",
  "-Wunused",
  "-language:implicitConversions"
)

// Scalafix config:
ThisBuild / scalafixScalaBinaryVersion := "2.13"
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.4.0"
ThisBuild / scalafixDependencies += "com.github.vovapolu" %% "scaluzzi" % "0.1.12"

lazy val root = project
  .in(file("."))
  .settings(
    organization := "codes.quine.labo",
    name := "gen",
    version := "0.1.0-SNAPSHOT",
    console / initialCommands := """
      |import scala.concurrent.ExecutionContext.Implicits.global
      |
      |import codes.quine.labo.gen._
      |import codes.quine.labo.gen.data._
      |import codes.quine.labo.gen.random._
      |import codes.quine.labo.gen.util._
      """.stripMargin,
    Compile / console / scalacOptions -= "-Wunused",
    // Scaladoc options:
    Compile / doc / scalacOptions ++= Seq(
      "-groups"
    ),
    // Set URL mapping of scala standard API for Scaladoc.
    apiMappings ++= scalaInstance.value.libraryJars
      .filter(file => file.getName.startsWith("scala-library") && file.getName.endsWith(".jar"))
      .map(_ -> url(s"http://www.scala-lang.org/api/${scalaVersion.value}/"))
      .toMap,
    // Settings for test:
    libraryDependencies += "io.monix" %% "minitest" % "2.8.2" % Test,
    testFrameworks += new TestFramework("minitest.runner.Framework"),
    doctestTestFramework := DoctestTestFramework.Minitest
  )
