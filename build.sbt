name := "Scales"

version := "1.0"

scalacOptions := Seq("-deprecation", "-feature", "-language:postfixOps")

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.4" % "test"
