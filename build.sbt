name := "scala_cats"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.0"

libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.5" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-Ypartial-unification"
)
