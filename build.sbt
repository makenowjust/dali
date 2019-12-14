import xerial.sbt.Sonatype._

val minitestVersion = "2.7.0"
val catsVersion = "2.0.0"

ThisBuild / scalaVersion := "2.13.1"
ThisBuild / scalacOptions ++= Seq(
  "-encoding",
  "UTF-8",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-Xlint",
  "-Ymacro-annotations"
)
ThisBuild / turbo := true

val commonSettings = Seq(
  organization := "codes.quine",
  Compile / console / scalacOptions += "-Ywarn-unused:-imports,_",
  Test / console / scalacOptions += "-Ywarn-unused:-imports,_",
  Compile / doc / scalacOptions ++= Seq("-diagrams", "-diagrams-max-classes", "10"),
  resolvers += Resolver.sonatypeRepo("releases"),
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full),
  sonatypeProjectHosting := Some(
    GitHubHosting(user = "MakeNowJust", repository = "dali", email = "make.just.on@gmail.com")
  ),
  licenses := Seq("MIT" -> url("https://opensource.org/licenses/MIT"))
)

lazy val root = project
  .in(file("."))
  .settings(name := "dali")
  .dependsOn(core, cats)
  .aggregate(core, cats)

lazy val core = project
  .in(file("modules/core"))
  .settings(
    name := "dali-core",
    description := "dali-core provides the basic classes for generic programming",
    libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "io.monix" %% "minitest" % minitestVersion % Test,
    testFrameworks += new TestFramework("minitest.runner.Framework"),
    commonSettings
  )

lazy val cats = project
  .in(file("modules/cats"))
  .settings(
    name := "dali-cats",
    description := "dali-cats provides auto-derivations for cats typeclasses",
    libraryDependencies += "org.typelevel" %% "cats-core" % catsVersion,
    libraryDependencies += "org.typelevel" %% "cats-laws" % catsVersion % Test,
    libraryDependencies += "io.monix" %% "minitest" % minitestVersion % Test,
    libraryDependencies += "io.monix" %% "minitest-laws" % minitestVersion % Test,
    testFrameworks += new TestFramework("minitest.runner.Framework"),
    commonSettings
  )
  .dependsOn(core, core % "test->test")
