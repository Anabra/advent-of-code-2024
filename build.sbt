ThisBuild / scalaVersion := "3.5.0"
ThisBuild / organization := "adventofcode2024"

lazy val uereliusMinusculus = (project in file("."))
  .settings(
    name := "advent-of-code-2024",
    run / fork := true,
    libraryDependencies ++= Seq(
    ),
  )
