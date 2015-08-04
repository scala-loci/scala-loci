import sbt._
import Keys._

object RetierBuild extends Build {
  val defaultSettings = Defaults.coreDefaultSettings ++ Seq(
    scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked", "-Xlint")
  )

  val macroparadise = Seq(
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
  )

  val macrodeclaration = Seq(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scalamacros" %% "resetallattrs" % "1.0.0-M1"
    )
  )

  val upickle = Seq(
    libraryDependencies += "com.lihaoyi" %% "upickle" % "0.3.4"
  )

  val nopublish = Seq(
    publishArtifact := false
  )

  lazy val retier = Project(
    id = "retier",
    base = file("."),
    settings = defaultSettings ++ nopublish
  ) aggregate (retierCore, retierMarshallableUpickle)

  lazy val retierCore = Project(
    id = "retier-core",
    base = file("retier-core"),
    settings = defaultSettings ++ SourceGenerator.usingExpressions ++ macroparadise ++ macrodeclaration
  )

  lazy val retierMarshallableUpickle = Project(
    id = "retier-marshallable-upickle",
    base = file("retier-marshallable-upickle"),
    settings = defaultSettings ++ upickle
  ) dependsOn (retierCore)
}
