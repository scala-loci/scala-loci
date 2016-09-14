scalaVersion in ThisBuild := "2.11.8"

version in ThisBuild := "0.0.1-SNAPSHOT"

organization in ThisBuild := "de.tuda.stg"

scalacOptions in ThisBuild ++= Seq("-feature", "-deprecation", "-unchecked", "-Xlint")


val macroparadise = addCompilerPlugin(
  "org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

val macrodeclaration = libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scalamacros" %% "resetallattrs" % "1.0.0")

val rescala = libraryDependencies +=
  "de.tuda.stg" %%% "rescala" % "0.0.0"

val upickle = libraryDependencies +=
  "com.lihaoyi" %%% "upickle" % "0.4.1"

val akkaHttp = libraryDependencies +=
  "com.typesafe.akka" %% "akka-http-experimental" % "2.4.8"

val play = libraryDependencies +=
  "com.typesafe.play" %% "play" % "2.5.4"

val scalajsDom = libraryDependencies +=
  "org.scala-js" %%%! "scalajs-dom" % "0.9.1"


def preventPublication(project: Project) = project settings (
  publish := { },
  publishLocal := { },
  publishArtifact := false,
  packagedArtifacts := Map.empty)


lazy val retier = preventPublication(project
  in file(".")
  aggregate (retierJVM, retierJS))

lazy val retierJVM = preventPublication(project
  in file(".jvm")
  aggregate (retierCoreJVM, retierArchitecturesBasicJVM,
             retierSerializableUpickleJVM,
             retierTransmitterBasicJVM, retierTransmitterRescalaJVM,
             retierNetworkTcpJVM, retierNetworkWsJVM, retierNetworkWsPlayJVM,
             retierNetworkWebRtcJVM))

lazy val retierJS = preventPublication(project
  in file(".js")
  aggregate (retierCoreJS, retierArchitecturesBasicJS,
             retierSerializableUpickleJS,
             retierTransmitterBasicJS, retierTransmitterRescalaJS,
             retierNetworkTcpJS, retierNetworkWsJS, retierNetworkWsPlayJS,
             retierNetworkWebRtcJS))


lazy val retierCore = (crossProject
  crossType CrossType.Full
  in file("retier-core")
  settings (normalizedName := "retier-core",
            SourceGenerator.transmittableTuples,
            SourceGenerator.valueTypesHigherKinds,
            macroparadise, macrodeclaration))

lazy val retierCoreJVM = retierCore.jvm
lazy val retierCoreJS = retierCore.js


lazy val retierArchitecturesBasic = (crossProject
  crossType CrossType.Pure
  in file("retier-architectures-basic")
  settings (normalizedName := "retier-architectures-basic",
            macroparadise)
  dependsOn retierCore)

lazy val retierArchitecturesBasicJVM = retierArchitecturesBasic.jvm
lazy val retierArchitecturesBasicJS = retierArchitecturesBasic.js


lazy val retierSerializableUpickle = (crossProject
  crossType CrossType.Pure
  in file("retier-serializable-upickle")
  settings (normalizedName := "retier-serializable-upickle",
            upickle)
  dependsOn retierCore)

lazy val retierSerializableUpickleJVM = retierSerializableUpickle.jvm
lazy val retierSerializableUpickleJS = retierSerializableUpickle.js


lazy val retierTransmitterBasic = (crossProject
  crossType CrossType.Pure
  in file("retier-transmitter-basic")
  settings (normalizedName := "retier-transmitter-basic")
  dependsOn retierCore)

lazy val retierTransmitterBasicJVM = retierTransmitterBasic.jvm
lazy val retierTransmitterBasicJS = retierTransmitterBasic.js


lazy val retierTransmitterRescala = (crossProject
  crossType CrossType.Pure
  in file("retier-transmitter-rescala")
  settings (normalizedName := "retier-transmitter-rescala",
            SourceGenerator.signalDefaultTuples, rescala)
  dependsOn retierCore)

lazy val retierTransmitterRescalaJVM = retierTransmitterRescala.jvm
lazy val retierTransmitterRescalaJS = retierTransmitterRescala.js


lazy val retierNetworkTcp = (crossProject
  crossType CrossType.Dummy
  in file("retier-network-tcp")
  settings (normalizedName := "retier-network-tcp")
  dependsOn retierCore)

lazy val retierNetworkTcpJVM = retierNetworkTcp.jvm
lazy val retierNetworkTcpJS = retierNetworkTcp.js


lazy val retierNetworkWs = (crossProject
  crossType CrossType.Dummy
  in file("retier-network-ws-akka")
  settings (normalizedName := "retier-network-ws-akka",
            akkaHttp, scalajsDom)
  dependsOn retierCore)

lazy val retierNetworkWsJVM = retierNetworkWs.jvm
lazy val retierNetworkWsJS = retierNetworkWs.js


lazy val retierNetworkWsPlay = (crossProject
  crossType CrossType.Dummy
  in file("retier-network-ws-akka-play")
  settings (normalizedName := "retier-network-ws-akka-play",
            play)
  dependsOn retierNetworkWs)

lazy val retierNetworkWsPlayJVM = retierNetworkWsPlay.jvm
lazy val retierNetworkWsPlayJS = retierNetworkWsPlay.js


lazy val retierNetworkWebRtc = (crossProject
  crossType CrossType.Full
  in file("retier-network-webrtc")
  settings (normalizedName := "retier-network-webrtc",
            scalajsDom)
  dependsOn retierCore)

lazy val retierNetworkWebRtcJVM = retierNetworkWebRtc.jvm
lazy val retierNetworkWebRtcJS = retierNetworkWebRtc.js