Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / githubOwner := "MakeNowJust-Labo"
ThisBuild / githubRepository := "hariko"

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
ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.5.0"
ThisBuild / scalafixDependencies += "com.github.vovapolu" %% "scaluzzi" % "0.1.17"

lazy val root = project
  .in(file("."))
  .settings(publish / skip := true)
  .aggregate(core, minitest)

def moduleSettings(moduleName: String) =
  Seq(
    organization := "codes.quine.labo",
    name := s"hariko-$moduleName",
    version := "0.1.1-SNAPSHOT",
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
    libraryDependencies += "io.monix" %% "minitest" % "2.9.3" % Test,
    testFrameworks += new TestFramework("minitest.runner.Framework"),
    doctestTestFramework := DoctestTestFramework.Minitest,
    // Surpress warnings in doctest generated files.
    libraryDependencies ++= Seq(
      compilerPlugin("com.github.ghik" % "silencer-plugin" % "1.7.2" cross CrossVersion.full),
      "com.github.ghik" % "silencer-lib" % "1.7.2" % Provided cross CrossVersion.full
    ),
    scalacOptions += "-P:silencer:globalFilters=toVoid is never used"
  )

lazy val core = project
  .in(file("modules/hariko-core"))
  .settings(
    moduleSettings("core"),
    console / initialCommands := """
      |import scala.concurrent.ExecutionContext.Implicits.global
      |
      |import codes.quine.labo.hariko._
      |import codes.quine.labo.hariko.data._
      |import codes.quine.labo.hariko.random._
      |import codes.quine.labo.hariko.util._
      """.stripMargin,
    // To avoid circular dependency, it copies `HarikoChecker.scala` from `hariko-minitest` before testing.
    Test / sourceGenerators += Def.task {
      val base =
        baseDirectory.value / "../hariko-minitest/src/main/scala/codes/quine/labo/hariko/minitest/HarikoChecker.scala"
      val gen = (Test / sourceManaged).value / "codes/quine/labo/hariko/HarikoChecker.scala"
      if (IO.getModifiedTimeOrZero(gen) < IO.getModifiedTimeOrZero(base)) {
        IO.write(gen, IO.read(base).replace("package minitest\n", ""))
        IO.setModifiedTimeOrFalse(gen, IO.getModifiedTimeOrZero(base))
      }
      Seq(gen)
    }.taskValue
  )

lazy val minitest = project
  .in(file("modules/hariko-minitest"))
  .settings(
    moduleSettings("minitest"),
    // Dependencies:
    libraryDependencies += "io.monix" %% "minitest" % "2.9.3" % Provided
  )
  .dependsOn(core)
