name := """SchiffeVersenken"""

version := "1.0"

lazy val root = (project in file(".")).enablePlugins(PlayJava, SbtWeb, PlayScala)

scalaVersion := "2.11.5"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  ws
)

unmanagedResourceDirectories in Test <+=  baseDirectory ( _ /"target/web/public/test" )

libraryDependencies ++= List(
  "com.typesafe.slick" %% "slick" % "2.1.0",
  "org.slf4j" % "slf4j-nop" % "1.6.4",
  "com.h2database" % "h2" % "1.3.175",
  "org.scalatest" %% "scalatest" % "2.1.6" % "test"
)
