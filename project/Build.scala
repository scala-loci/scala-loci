import sbt._
import Keys._

object RetierBuild extends Build {
  val defaultSettings = Defaults.coreDefaultSettings ++ Seq(
    scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked", "-Xlint")
  )

  val macroparadise = Seq(
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  )

  val macrodeclaration = Seq(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scalamacros" %% "resetallattrs" % "1.0.0-M1"
    )
  )

  val rescala = Seq(
    libraryDependencies += "de.tuda.stg" %% "rescala" % "0+"
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
  ) aggregate (
    retierCore, retierSerializableUpickle,
    retierTransmitterBasic, retierTransmitterRescala,
    retierNetworkTCP)

  lazy val retierCore = Project(
    id = "retier-core",
    base = file("retier-core"),
    settings = defaultSettings ++
      SourceGenerator.transmittableTuples ++ macroparadise ++ macrodeclaration
  )

  lazy val retierSerializableUpickle = Project(
    id = "retier-serializable-upickle",
    base = file("retier-serializable-upickle"),
    settings = defaultSettings ++ upickle
  ) dependsOn retierCore

  lazy val retierTransmitterBasic = Project(
    id = "retier-transmitter-basic",
    base = file("retier-transmitter-basic"),
    settings = defaultSettings
  ) dependsOn retierCore

  lazy val retierTransmitterRescala = Project(
    id = "retier-transmitter-rescala",
    base = file("retier-transmitter-rescala"),
    settings = defaultSettings ++ rescala
  ) dependsOn retierCore

  lazy val retierNetworkTCP = Project(
    id = "retier-network-tcp",
    base = file("retier-network-tcp"),
    settings = defaultSettings
  ) dependsOn retierCore
}
