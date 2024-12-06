ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.1"

lazy val root = (project in file("."))
  .settings(
    name                := "advent2024",
    libraryDependencies += "com.lihaoyi"   %% "os-lib"    % "0.11.3",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.12.0",
    libraryDependencies += "org.scalameta" %% "munit"     % "1.0.3" % Test
  )
