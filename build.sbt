name := "lucinda"
scalaVersion := "2.12.1"

enablePlugins(
  AssemblyPlugin,
  BuildInfoPlugin,
  DockerPlugin,
  GitVersioning
)

libraryDependencies ++= Seq(
  "asuna" %% "common" % "1.4.2",
  "com.github.etaty" %% "rediscala" % "1.8.0",

  "org.scalacheck" %% "scalacheck" % "1.13.3" % "test",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

testOptions in Test += Tests.Argument("-oDF")
