scalaVersion := "2.13.16"
version := "0.1"
name := "functional-data-structures-and-algorithms"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.2.19",
  "org.scalatest" %% "scalatest" % "3.2.19" % "test"
)

// Add some modern compiler options for better warnings and optimizations
scalacOptions ++= Seq(
  "-deprecation",
  "-feature", 
  "-unchecked",
  "-Xfatal-warnings"
)
