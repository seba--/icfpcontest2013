name := "icfpcontest2013"

version := "0.0.0"

scalaVersion := "2.10.2"

scalacOptions ++= List(
	"-deprecation",
	"-encoding", "UTF-8",
	"-unchecked",
	"-feature",
	"-target:jvm-1.6",
	"-language:implicitConversions",
	"-language:reflectiveCalls",
	"-Xlint"
)

resolvers ++= Seq()

libraryDependencies ++= Seq(
    "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
	"org.scalaj" %% "scalaj-http" % "0.3.9" exclude("junit", "junit"),
	"com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.1.3"
)
