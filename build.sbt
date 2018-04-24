enablePlugins(GitVersioning)

git.useGitDescribe in ThisBuild := true

scalaVersion in ThisBuild := "2.11.8"

organization in ThisBuild := "de.tuda.stg"

licenses in ThisBuild += "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")

scalacOptions in ThisBuild ++= Seq("-feature", "-deprecation", "-unchecked", "-Xlint")


val macroparadise = addCompilerPlugin(
  "org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.patch)

val macrodeclaration = libraryDependencies +=
  scalaOrganization.value % "scala-reflect" % scalaVersion.value % "provided"

val scalatest = libraryDependencies +=
  "org.scalatest" %%% "scalatest" % "3.0.0" % "test"

val retypecheckRepo =
  resolvers += Resolver.bintrayRepo("pweisenburger", "maven")

val retypecheck = libraryDependencies +=
  "de.tuda.stg" %% "retypecheck" % "0.1.0"

val rescalaRepo =
  resolvers += Resolver.bintrayRepo("rmgk", "maven")

val rescala = libraryDependencies +=
  "de.tuda.stg" %%% "rescala" % "0.19.0"

val upickle = libraryDependencies +=
  "com.lihaoyi" %%% "upickle" % "0.4.4"

val akkaHttp = libraryDependencies +=
  "com.typesafe.akka" %% "akka-http" % "10.0.5"

val play = libraryDependencies +=
  "com.typesafe.play" %% "play" % "2.5.14"

val scalajsDom = libraryDependencies +=
  "org.scala-js" %%%! "scalajs-dom" % "0.9.1"


def preventPublication(project: Project) = project settings (
  publish := { },
  publishLocal := { },
  publishArtifact := false,
  packagedArtifacts := Map.empty)


lazy val loci = preventPublication(project
  in file(".")
  aggregate (lociJVM, lociJS))

lazy val lociJVM = preventPublication(project
  in file(".jvm")
  aggregate (lociCoreJVM, lociArchitecturesBasicJVM,
             lociSerializableUpickleJVM,
             lociTransmitterBasicJVM, lociTransmitterRescalaJVM,
             lociNetworkTcpJVM, lociNetworkWsJVM, lociNetworkWsPlayJVM,
             lociNetworkWebRtcJVM))

lazy val lociJS = preventPublication(project
  in file(".js")
  aggregate (lociCoreJS, lociArchitecturesBasicJS,
             lociSerializableUpickleJS,
             lociTransmitterBasicJS, lociTransmitterRescalaJS,
             lociNetworkTcpJS, lociNetworkWsJS, lociNetworkWsPlayJS,
             lociNetworkWebRtcJS))


lazy val lociCore = (crossProject
  crossType CrossType.Full
  in file("scala-loci-core")
  settings (normalizedName := "scala-loci-core",
            SourceGenerator.transmittableTuples,
            SourceGenerator.valueTypesHigherKinds,
            retypecheckRepo, retypecheck,
            macroparadise, macrodeclaration, scalatest))

lazy val lociCoreJVM = lociCore.jvm
lazy val lociCoreJS = lociCore.js


lazy val lociArchitecturesBasic = (crossProject
  crossType CrossType.Pure
  in file("scala-loci-architectures-basic")
  settings (normalizedName := "scala-loci-architectures-basic",
            macroparadise)
  dependsOn lociCore)

lazy val lociArchitecturesBasicJVM = lociArchitecturesBasic.jvm
lazy val lociArchitecturesBasicJS = lociArchitecturesBasic.js


lazy val lociSerializableUpickle = (crossProject
  crossType CrossType.Pure
  in file("scala-loci-serializable-upickle")
  settings (normalizedName := "scala-loci-serializable-upickle",
            upickle)
  dependsOn lociCore)

lazy val lociSerializableUpickleJVM = lociSerializableUpickle.jvm
lazy val lociSerializableUpickleJS = lociSerializableUpickle.js


lazy val lociTransmitterBasic = (crossProject
  crossType CrossType.Pure
  in file("scala-loci-transmitter-basic")
  settings (normalizedName := "scala-loci-transmitter-basic")
  dependsOn lociCore)

lazy val lociTransmitterBasicJVM = lociTransmitterBasic.jvm
lazy val lociTransmitterBasicJS = lociTransmitterBasic.js


lazy val lociTransmitterRescala = (crossProject
  crossType CrossType.Pure
  in file("scala-loci-transmitter-rescala")
  settings (normalizedName := "scala-loci-transmitter-rescala",
            rescalaRepo, rescala)
  dependsOn lociCore)

lazy val lociTransmitterRescalaJVM = lociTransmitterRescala.jvm
lazy val lociTransmitterRescalaJS = lociTransmitterRescala.js


lazy val lociNetworkTcp = (crossProject
  crossType CrossType.Dummy
  in file("scala-loci-network-tcp")
  settings (normalizedName := "scala-loci-network-tcp")
  dependsOn lociCore)

lazy val lociNetworkTcpJVM = lociNetworkTcp.jvm
lazy val lociNetworkTcpJS = lociNetworkTcp.js


lazy val lociNetworkWs = (crossProject
  crossType CrossType.Dummy
  in file("scala-loci-network-ws-akka")
  settings (normalizedName := "scala-loci-network-ws-akka",
            akkaHttp, scalajsDom)
  dependsOn lociCore)

lazy val lociNetworkWsJVM = lociNetworkWs.jvm
lazy val lociNetworkWsJS = lociNetworkWs.js


lazy val lociNetworkWsPlay = (crossProject
  crossType CrossType.Dummy
  in file("scala-loci-network-ws-akka-play")
  settings (normalizedName := "scala-loci-network-ws-akka-play",
            play)
  dependsOn lociNetworkWs)

lazy val lociNetworkWsPlayJVM = lociNetworkWsPlay.jvm
lazy val lociNetworkWsPlayJS = lociNetworkWsPlay.js


lazy val lociNetworkWebRtc = (crossProject
  crossType CrossType.Full
  in file("scala-loci-network-webrtc")
  settings (normalizedName := "scala-loci-network-webrtc",
            scalajsDom)
  dependsOn lociCore)

lazy val lociNetworkWebRtcJVM = lociNetworkWebRtc.jvm
lazy val lociNetworkWebRtcJS = lociNetworkWebRtc.js
