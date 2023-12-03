val scala3Version = "3.3.0"

val circeVersion = "0.14.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "aoc-2022",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion) ++ Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )
