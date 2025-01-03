ThisBuild / scalaVersion := "3.5.0"
ThisBuild / organization := "adventofcode2024"

lazy val uereliusMinusculus = (project in file("."))
  .settings(
    name := "advent-of-code-2024",
    run / fork := true,
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.1.0",
      "guru.nidi" % "graphviz-java" % "0.18.1",
    ),
  )
