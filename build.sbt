organization := "xieyuheng"
name := "cicada"
version := "0.0.1"
scalaVersion := "2.13.0"

scalacOptions ++= Seq(
  "-deprecation",
  //   "-encoding", "UTF-8",
  //   "-unchecked",
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  // "-language:postfixOps",
  "-Ywarn-dead-code",
  //   "-Xfatal-warnings"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
)
