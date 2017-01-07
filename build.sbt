name := "lucinda"
organization := "io.asuna"
version := "0.1.0"
scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "io.asuna" %% "asunasan" % "0.9.4",

  // Deps
  "com.github.etaty" %% "rediscala" % "1.6.0",
  "com.websudos" %%  "phantom-dsl" % "1.29.5",  // cassandra

  // Testing
  "org.scalacheck" %% "scalacheck" % "1.13.3" % "test",
  "org.scalactic" %% "scalactic" % "3.0.0" % "test",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

mainClass in assembly := Some("io.asuna.lucinda.Main")

assemblyMergeStrategy in assembly := {
  case x if x.endsWith("io.netty.versions.properties") => MergeStrategy.first
  case x if x contains "publicsuffix" => MergeStrategy.first
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}
assemblyJarName in assembly := "lucinda-assembly.jar"

awsProfile := "asuna"
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

lazy val root = (project in file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "buildinfo"
  )
