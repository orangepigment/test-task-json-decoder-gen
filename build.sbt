ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.15"

lazy val root = (project in file("."))
  .settings(
    name := "test-task-json-decoder-gen",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
  )
