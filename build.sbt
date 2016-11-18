name := "lucinda"
organization := "io.asuna"
version := "0.1.0"
scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  // Asuna standard lib
  "io.asuna" %% "asunasan" % "0.1.1-SNAPSHOT",

  // Proto stuff
  "com.trueaccord.scalapb" %% "scalapb-runtime" % "0.5.39",
  "com.trueaccord.scalapb" %% "scalapb-runtime-grpc" % "0.5.39",
  "io.grpc" % "grpc-netty" % "1.0.1",

  // Scalatest
  "org.scalactic" %% "scalactic" % "3.0.0",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.3" % "test", // prop tests

  "com.github.scopt" %% "scopt" % "3.5.0",
  "org.scalaz" %% "scalaz-core" % "7.2.7",
  "com.github.etaty" %% "rediscala" % "1.6.0",
  "com.lihaoyi" %% "upickle" % "0.4.3",

  // Phantom
  "com.websudos" %%  "phantom-dsl" % "1.29.5"
)

mainClass in assembly := Some("io.asuna.lucinda.Main")

assemblyMergeStrategy in assembly := {
  case x if x.endsWith("io.netty.versions.properties") => MergeStrategy.first
  case x if x contains "publicsuffix" => MergeStrategy.first
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

s3region := com.amazonaws.services.s3.model.Region.US_West
s3acl := com.amazonaws.services.s3.model.CannedAccessControlList.AuthenticatedRead

// Resolver
resolvers ++= Seq[Resolver](
  Resolver.bintrayRepo("websudos", "oss-releases"),
  s3resolver.value("Aincrad", s3("aincrad.asuna.io"))
)

// testing
testOptions in Test += Tests.Argument("-oDF")

// Needed for ENSIME
scalaVersion in ThisBuild := "2.11.8"
