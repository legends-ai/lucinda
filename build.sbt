name := "lucinda"
organization := "asuna"
scalaVersion := "2.11.8"

  // Resolver
resolvers ++= Seq[Resolver](
  Resolver.bintrayRepo("websudos", "oss-releases"),
  Resolver.bintrayRepo("stew", "snapshots"),
  "Aincrad" at "s3://aincrad.asuna.io"
)

libraryDependencies ++= Seq(
  "asuna" %% "common" % "1.1.16",
  "com.github.etaty" %% "rediscala" % "1.8.0",

  // Testing
  "org.scalacheck" %% "scalacheck" % "1.13.3" % "test",
  "org.scalactic" %% "scalactic" % "3.0.0" % "test",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

mainClass in assembly := Some("asuna.lucinda.Main")
assemblyJarName in assembly := "lucinda-assembly.jar"
assemblyMergeStrategy in assembly := {
  case x if x.endsWith("io.netty.versions.properties") => MergeStrategy.first
  case x if x contains "publicsuffix" => MergeStrategy.first
  case x => (assemblyMergeStrategy in assembly).value(x)
}

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

val base = "096202052535.dkr.ecr.us-east-1.amazonaws.com"
imageNames in docker := Seq(
  // Sets the latest tag
  ImageName(s"${base}/${name.value}:latest"),
  ImageName(s"${base}/${name.value}:${version.value}")
)

lazy val root = (project in file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "buildinfo"
  )
