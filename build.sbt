ThisBuild / version := "0.9.0"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "exercise10"
  )

ThisBuild / libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % Test