name := "lucinda"
scalaVersion := "2.12.1"
ensimeScalaVersion in ThisBuild := "2.11.8"

enablePlugins(
  AssemblyPlugin,
  BuildInfoPlugin,
  DockerPlugin,
  GitVersioning
)

libraryDependencies ++= Seq(
  "asuna" %% "common" % "2.1.14",
  "com.github.etaty" %% "rediscala" % "1.8.0",
  "ch.qos.logback" % "logback-classic" % "1.1.9",
  "net.logstash.logback" % "logstash-logback-encoder" % "4.8",
  "org.scalacheck" %% "scalacheck" % "1.13.3" % "test",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

testOptions in Test += Tests.Argument("-oDF")
