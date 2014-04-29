resolvers ++= Seq(
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "Typesafe Snapshots" at "http://repo.typesafe.com/typesafe/snapshots/",
  "Typesafe Release Repository" at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.0-RC4" withSources() withJavadoc()

libraryDependencies += "com.typesafe.akka" %% "akka-cluster" % "2.3.0-RC4" withSources() withJavadoc()

libraryDependencies += "com.typesafe.akka" %% "akka-remote" % "2.3.0-RC4" withSources() withJavadoc()

libraryDependencies += "com.typesafe.akka" %% "akka-contrib" % "2.3.0-RC4" withSources() withJavadoc()

libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % "2.3.0-RC4" % "test" withSources() withJavadoc()
