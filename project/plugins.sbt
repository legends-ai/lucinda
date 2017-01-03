resolvers += "Era7 maven releases" at "https://s3-eu-west-1.amazonaws.com/releases.era7.com"

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.3")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.6.1")
addSbtPlugin("ohnosequences" % "sbt-s3-resolver" % "0.15.0")
addSbtPlugin("se.marcuslonnberg" % "sbt-docker" % "1.4.0")
