name := "tetriq"

version := "0.1"

//scalaVersion := "2.10.0-M6"

libraryDependencies ++= Seq(
    "junit" % "junit" % "4.10" % "test",
    "org.scalatest" %% "scalatest" % "1.8" % "test",
    "org.mockito" % "mockito-all" % "1.9.0" % "test",
    "ch.qos.logback" % "logback-classic" % "1.0.7"
)

resolvers += "eclipse" at "http://mirror.csclub.uwaterloo.ca/eclipse/rt/eclipselink/maven.repo/"

scalacOptions += "-deprecation"

EclipseKeys.withSource := true

