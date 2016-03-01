name := "csd-base"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % "0.3.0",
  "io.circe" %% "circe-generic" % "0.3.0",
  "io.circe" %% "circe-parser" % "0.3.0",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test"
)
    