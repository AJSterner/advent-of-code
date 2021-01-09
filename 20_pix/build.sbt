lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.aoc2020",
      scalaVersion := "2.13.3"
    )),
    name := "20-pix"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % Test
