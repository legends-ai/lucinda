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
  "org.scalacheck" %% "scalacheck" % "1.13.3" % "test", // prop tests
  "org.scalactic" %% "scalactic" % "3.0.0" % "test",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",

  "com.github.etaty" %% "rediscala" % "1.6.0",
  "com.github.scopt" %% "scopt" % "3.5.0",
  "com.lihaoyi" %% "upickle" % "0.4.3",
  "org.scalaz" %% "scalaz-core" % "7.2.7",
  "org.typelevel" %% "scalaz-outlaws" % "0.2",

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
  Resolver.bintrayRepo("stew", "snapshots"),
  s3resolver.value("Aincrad", s3("aincrad.asuna.io"))
)

// testing
testOptions in Test += Tests.Argument("-oDF")

// Needed for ENSIME
scalaVersion in ThisBuild := "2.11.8"

// Docker stuff
enablePlugins(DockerPlugin)

dockerfile in docker := {
  // The assembly task generates a fat JAR file
  val artifact: File = assembly.value
  val artifactTargetPath = s"/app/${artifact.name}"

  new Dockerfile {
    from("java")
    add(artifact, artifactTargetPath)
    entryPoint("java", "-jar", artifactTargetPath)
  }
}

val base = "096202052535.dkr.ecr.us-west-2.amazonaws.com"
imageNames in docker := Seq(
  // Sets the latest tag
  ImageName(s"${base}/${name.value}:latest"),
  ImageName(s"${base}/${name.value}:${version.value}")
)
