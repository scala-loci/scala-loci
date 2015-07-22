import sbt._
import Keys._

object RetierBuild extends Build {
  val defaultSettings = Defaults.coreDefaultSettings ++ Seq(
    scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked", "-Xlint")
  )

  val macroparadise = Seq(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scalamacros" %% "resetallattrs" % "1.0.0-M1"
    ),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
  )

  val nopublish = Seq(
    publishArtifact := false
  )

  lazy val retier = Project(
    id = "retier",
    base = file("."),
    settings = defaultSettings ++ nopublish
  ) aggregate (retierCore)

  lazy val retierCore = Project(
    id = "retier-core",
    base = file("retier-core"),
    settings = defaultSettings ++ SourceGenerator.usingExpressions ++ macroparadise
  )
}
