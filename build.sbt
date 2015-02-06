name := """SchiffeVersenken"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayJava, SbtWeb, PlayScala)

scalaVersion := "2.11.5"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  ws
)

unmanagedResourceDirectories in Test <+=  baseDirectory ( _ /"target/web/public/test" )