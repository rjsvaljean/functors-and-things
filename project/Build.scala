import sbt._
import Keys._

object FunctorsAndThingsBuild extends Build {
  lazy val main = Project(
    id = "main",
    base = file(".")
  )
}
