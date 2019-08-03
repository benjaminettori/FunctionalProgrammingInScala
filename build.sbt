name := "FunctionalProgrammingInScala"

version := "1.0"

scalaVersion := "2.12.2"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.5.12",
  "org.typelevel" %% "cats-effect" % "1.3.1",
  "com.softwaremill.quicklens" %% "quicklens" % "1.4.12",
  "com.typesafe.akka" %% "akka-testkit" % "2.5.12" % Test
)
