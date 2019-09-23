organization := "xieyuheng"
name := "cicada"
version := "0.0.1"
scalaVersion := "2.12.9"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-unchecked",
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-Xfatal-warnings",
)

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "upickle" % "0.7.1",
  "com.lihaoyi" %% "os-lib" % "0.3.0",
)

enablePlugins(JavaAppPackaging)
publishArtifact in packageDoc := false
publishArtifact in packageSrc := false

// enablePlugins(MdocPlugin)
// mdocVariables := Map(
//   "VERSION" -> version.value,
// )

// resolvers += Resolver.sonatypeRepo("releases")
// addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")
// addCompilerPlugin("org.typelevel" % "kind-projector" % "0.10.3" cross CrossVersion.binary)
