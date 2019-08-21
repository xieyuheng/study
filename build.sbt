organization := "xieyuheng"
name := "cicada"
version := "0.0.1"
scalaVersion := "2.12.9"

micrositeGithubOwner := "xieyuheng"
micrositeGithubRepo := "cicada"

scalacOptions ++= Seq(
  "-opt:l:method",
  "-opt:l:inline",
  "-opt-inline-from:**",
  "-deprecation",
  "-encoding", "UTF-8",
  "-unchecked",
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-Xfatal-warnings",
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
)

enablePlugins(JavaAppPackaging)
enablePlugins(TutPlugin)

resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")
addCompilerPlugin("org.typelevel" % "kind-projector" % "0.10.3" cross CrossVersion.binary)
