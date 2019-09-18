import xerial.sbt.Sonatype._

ThisBuild / publishMavenStyle := true

ThisBuild / sonatypeProfileName := "codes.quine"

ThisBuild / sonatypeProjectHosting := Some(
  GitHubHosting(user = "MakeNowJust", repository = "dali", email = "make.just.on@gmail.com")
)
ThisBuild / licenses := Seq("MIT" -> url("https://opensource.org/licenses/MIT"))

ThisBuild / publishTo := sonatypePublishToBundle.value
