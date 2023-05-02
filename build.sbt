ThisBuild / version := "0.1.0-SNAPSHOT"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "mddScala"
  )
