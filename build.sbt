import xerial.sbt.Sonatype._

ThisBuild / scalaVersion := "2.13.0"
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
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
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
    libraryDependencies += "io.monix" %% "minitest" % "2.6.0" % Test,
    testFrameworks += new TestFramework("dali.MinitestFramework"),
    commonSettings
  )

lazy val cats = project
  .in(file("modules/cats"))
  .settings(
    name := "dali-cats",
    description := "dali-cats provides auto-derivations for cats typeclasses",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0",
    libraryDependencies += "org.typelevel" %% "cats-laws" % "2.0.0" % Test,
    libraryDependencies += "io.monix" %% "minitest" % "2.6.0" % Test,
    libraryDependencies += "io.monix" %% "minitest-laws" % "2.6.0" % Test,
    testFrameworks += new TestFramework("dali.MinitestFramework"),
    commonSettings
  )
  .dependsOn(core, core % "test->test")

lazy val singleton = project
  .in(file("modules/singleton"))
  .settings(
    name := "dali-singleton",
    description := "dali-singleton provides type-level singleton operations",
    libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "io.monix" %% "minitest" % "2.6.0" % Test,
    testFrameworks += new TestFramework("dali.MinitestFramework"),
    commonSettings
  )
  .dependsOn(core % "test->test")
