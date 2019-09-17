ThisBuild / scalaVersion := "2.13.0"
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-unchecked",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-Xlint",
  "-Ymacro-annotations"
)

val commonSettings = Seq(
  Compile / console / scalacOptions += "-Ywarn-unused:-imports,_",
  Test / console / scalacOptions += "-Ywarn-unused:-imports,_",
  Compile / doc / scalacOptions ++= Seq("-diagrams", "-diagrams-max-classes", "10"),
  resolvers += Resolver.sonatypeRepo("releases"),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0"),
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "labo",
  )
  .dependsOn(core, cats)
  .aggregate(core, cats)

lazy val core = project
  .in(file("modules/core"))
  .settings(
    name := "dal-core",
    libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "io.monix" %% "minitest" % "2.6.0" % Test,
    testFrameworks += new TestFramework("dali.MinitestFramework"),
    commonSettings
  )

lazy val cats = project
  .in(file("modules/cats"))
  .settings(
    name := "dali-cats",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0",
    libraryDependencies += "org.typelevel" %% "cats-laws" % "2.0.0" % Test,
    libraryDependencies += "io.monix" %% "minitest" % "2.6.0" % Test,
    libraryDependencies += "io.monix" %% "minitest-laws" % "2.6.0" % Test,
    testFrameworks += new TestFramework("dali.MinitestFramework"),
    commonSettings
  )
  .dependsOn(core, core % "test->test")
