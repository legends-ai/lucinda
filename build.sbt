addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

name := "lucinda"
scalaVersion := "2.12.2"

enablePlugins(
  AssemblyPlugin,
  BuildInfoPlugin,
  DockerPlugin,
  GitVersioning
)

libraryDependencies ++= Seq(
  "asuna" %% "common" % "3.2.4",
  "ch.qos.logback" % "logback-classic" % "1.1.9",
  "net.logstash.logback" % "logstash-logback-encoder" % "4.8",

  "org.scalacheck" %% "scalacheck" % "1.13.3" % "test",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

testOptions in Test += Tests.Argument("-oDF")
