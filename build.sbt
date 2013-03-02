name := "functorsAndThings"

version := "0.0.1"

scalaVersion := "2.9.2"

crossScalaVersions := Seq("2.9.2", "2.10.0")

libraryDependencies <++= scalaVersion(specs2Dependencies(_))

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"
