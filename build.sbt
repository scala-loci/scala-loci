enablePlugins(GitVersioning)

git.useGitDescribe in ThisBuild := true

scalaVersion in ThisBuild := "2.12.4"

crossScalaVersions in ThisBuild := Seq("2.11.12", "2.12.4")

organization in ThisBuild := "de.tuda.stg"

licenses in ThisBuild += "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")

scalacOptions in ThisBuild ++= Seq("-feature", "-deprecation", "-unchecked", "-Xlint")


val macroparadise = addCompilerPlugin(
  "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch)

val macrodeclaration = libraryDependencies +=
  scalaOrganization.value % "scala-reflect" % scalaVersion.value % "provided"

val scalatest = libraryDependencies +=
  "org.scalatest" %%% "scalatest" % "3.0.4" % "test"

val retypecheckRepo =
  resolvers += Resolver.bintrayRepo("stg-tud", "maven")

val retypecheck = libraryDependencies +=
  "de.tuda.stg" %% "retypecheck" % "0.4.0"

val rescalaRepo =
  resolvers += Resolver.bintrayRepo("stg-tud", "maven")

val rescala = libraryDependencies +=
  "de.tuda.stg" %%% "rescala" % "0.20.0"

val upickle = libraryDependencies +=
  "com.lihaoyi" %%% "upickle" % "0.4.4"

val circe = libraryDependencies ++= Seq(
  "io.circe" %%% "circe-core" % "0.9.1",
  "io.circe" %%% "circe-parser" % "0.9.1")

val akkaHttp = libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % "[10.0,11.0)" % "provided",
  "com.typesafe.akka" %% "akka-stream" % "[2.4,3.0)" % "provided")

val play = libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % "[10.0,11.0)" % "provided",
  "com.typesafe.play" %% "play" % "[2.5,2.7)" % "provided")

val playDependencyOverrides = dependencyOverrides ++= Seq(
  "org.scala-lang.modules" %% "scala-java8-compat" % "[0.7,0.9)" % "provided",
  "com.typesafe.akka" %% "akka-http" % "[10.0,11.0)" % "provided",
  "com.typesafe.akka" %% "akka-stream" % "[2.4,3.0)" % "provided",
  "com.typesafe.akka" %% "akka-actor" % "[2.4,3.0)" % "provided")

val scalajsDom = libraryDependencies +=
  "org.scala-js" %%%! "scalajs-dom" % "0.9.4"


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
  aggregate (lociLangJVM, lociArchitecturesBasicJVM,
             lociSerializerUpickleJVM,
             lociSerializerCirceJVM,
             lociTransmitterRescalaJVM, lociLangTransmitterRescalaJVM,
             lociLangTransmitterBasicJVM,
             lociCommunicatorTcpJVM, lociCommunicatorWsJVM,
             lociCommunicatorWsPlayJVM, lociCommunicatorWebRtcJVM,
             lociCommunicationJVM))

lazy val lociJS = preventPublication(project
  in file(".js")
  aggregate (lociLangJS, lociArchitecturesBasicJS,
             lociSerializerUpickleJS,
             lociSerializerCirceJS,
             lociTransmitterRescalaJS, lociLangTransmitterRescalaJS,
             lociLangTransmitterBasicJS,
             lociCommunicatorTcpJS, lociCommunicatorWsJS,
             lociCommunicatorWsPlayJS, lociCommunicatorWebRtcJS,
             lociCommunicationJS))


lazy val lociLang = (crossProject
  crossType CrossType.Full
  in file("scala-loci-lang")
  settings (normalizedName := "scala-loci-lang",
            SourceGenerator.valueTypesHigherKinds,
            retypecheckRepo, retypecheck,
            macroparadise, macrodeclaration, scalatest)
  dependsOn lociCommunication)

lazy val lociLangJVM = lociLang.jvm
lazy val lociLangJS = lociLang.js


lazy val lociCommunication = (crossProject
  crossType CrossType.Full
  in file("scala-loci-communication")
  settings (normalizedName := "scala-loci-communication",
            SourceGenerator.transmittableTuples,
            SourceGenerator.functionsBindingBuilder,
            scalatest))

lazy val lociCommunicationJVM = lociCommunication.jvm
lazy val lociCommunicationJS = lociCommunication.js


lazy val lociArchitecturesBasic = (crossProject
  crossType CrossType.Pure
  in file("scala-loci-architectures-basic")
  settings (normalizedName := "scala-loci-architectures-basic",
            macroparadise)
  dependsOn lociLang)

lazy val lociArchitecturesBasicJVM = lociArchitecturesBasic.jvm
lazy val lociArchitecturesBasicJS = lociArchitecturesBasic.js


lazy val lociSerializerUpickle = (crossProject
  crossType CrossType.Pure
  in file("scala-loci-serializer-upickle")
  settings (normalizedName := "scala-loci-serializer-upickle",
            upickle)
  dependsOn lociCommunication)

lazy val lociSerializerUpickleJVM = lociSerializerUpickle.jvm
lazy val lociSerializerUpickleJS = lociSerializerUpickle.js


lazy val lociSerializerCirce = (crossProject
  crossType CrossType.Pure
  in file("scala-loci-serializer-circe")
  settings (normalizedName := "scala-loci-serializer-circe",
            circe)
  dependsOn lociCommunication)

lazy val lociSerializerCirceJVM = lociSerializerCirce.jvm
lazy val lociSerializerCirceJS = lociSerializerCirce.js


lazy val lociTransmitterRescala = (crossProject
  crossType CrossType.Pure
  in file("scala-loci-transmitter-rescala")
  settings (normalizedName := "scala-loci-transmitter-rescala",
            rescalaRepo, rescala)
  dependsOn lociCommunication)

lazy val lociTransmitterRescalaJVM = lociTransmitterRescala.jvm
lazy val lociTransmitterRescalaJS = lociTransmitterRescala.js


lazy val lociLangTransmitterRescala = (crossProject
  crossType CrossType.Pure
  in file("scala-loci-lang-transmitter-rescala")
  settings (normalizedName := "scala-loci-lang-transmitter-rescala")
  dependsOn (lociLang, lociTransmitterRescala))

lazy val lociLangTransmitterRescalaJVM = lociLangTransmitterRescala.jvm
lazy val lociLangTransmitterRescalaJS = lociLangTransmitterRescala.js


lazy val lociLangTransmitterBasic = (crossProject
  crossType CrossType.Pure
  in file("scala-loci-lang-transmitter-basic")
  settings (normalizedName := "scala-loci-lang-transmitter-basic")
  dependsOn lociLang)

lazy val lociLangTransmitterBasicJVM = lociLangTransmitterBasic.jvm
lazy val lociLangTransmitterBasicJS = lociLangTransmitterBasic.js


lazy val lociCommunicatorTcp = (crossProject
  crossType CrossType.Dummy
  in file("scala-loci-communicator-tcp")
  settings (normalizedName := "scala-loci-communicator-tcp")
  dependsOn lociCommunication)

lazy val lociCommunicatorTcpJVM = lociCommunicatorTcp.jvm
lazy val lociCommunicatorTcpJS = lociCommunicatorTcp.js


lazy val lociCommunicatorWs = (crossProject
  crossType CrossType.Dummy
  in file("scala-loci-communicator-ws-akka")
  settings (normalizedName := "scala-loci-communicator-ws-akka",
            akkaHttp, scalajsDom)
  dependsOn lociCommunication)

lazy val lociCommunicatorWsJVM = lociCommunicatorWs.jvm
lazy val lociCommunicatorWsJS = lociCommunicatorWs.js


lazy val lociCommunicatorWsPlay = (crossProject
  crossType CrossType.Dummy
  in file("scala-loci-communicator-ws-akka-play")
  settings (normalizedName := "scala-loci-communicator-ws-akka-play",
            play)
  dependsOn lociCommunicatorWs)

lazy val lociCommunicatorWsPlayJVM = lociCommunicatorWsPlay.jvm
lazy val lociCommunicatorWsPlayJS = lociCommunicatorWsPlay.js


lazy val lociCommunicatorWebRtc = (crossProject
  crossType CrossType.Full
  in file("scala-loci-communicator-webrtc")
  settings (normalizedName := "scala-loci-communicator-webrtc",
            scalajsDom)
  dependsOn lociCommunication)

lazy val lociCommunicatorWebRtcJVM = lociCommunicatorWebRtc.jvm
lazy val lociCommunicatorWebRtcJS = lociCommunicatorWebRtc.js

