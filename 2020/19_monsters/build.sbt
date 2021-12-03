lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.aoc.2020",
      scalaVersion := "2.13.3"
    )),
    name := "day19"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % Test