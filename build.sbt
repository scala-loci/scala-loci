enablePlugins(GitVersioning)

Global / excludeLintKeys += git.useGitDescribe

ThisBuild / git.useGitDescribe := true

ThisBuild / organization := "io.github.scala-loci"

ThisBuild / homepage := Some(url("https://scala-loci.github.io/"))

ThisBuild / licenses += "Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")

ThisBuild / scalacOptions ++= {
  if (`is 3+`(scalaVersion.value))
    Seq("-feature", "-deprecation", "-unchecked")
  else
    Seq("-feature", "-deprecation", "-unchecked", "-Xlint", "-language:higherKinds")
}

ThisBuild / crossScalaVersions := Seq("2.11.12", "2.12.15", "2.13.10", "3.2.2")

ThisBuild / scalaVersion := {
  val versions = (ThisBuild / crossScalaVersions).value
  val version = Option(System.getenv("SCALA_VERSION")) getOrElse versions(versions.size - 2)
  versions.reverse find { _ startsWith version } getOrElse versions.last
}


val build = taskKey[Unit]("Builds the system")

val aggregatedProjects = ScopeFilter(inAggregates(ThisProject, includeRoot = false))

def taskSequence(tasks: TaskKey[_]*) =
  Def.sequential(tasks map { task => Def.task { Def.unit(task.value) } all aggregatedProjects })


val macroparadise = Seq(
  scalacOptions ++= {
    if (`is 2.13+`(scalaVersion.value) && !`is 3+`(scalaVersion.value))
      Seq("-Ymacro-annotations")
    else
      Seq.empty
  },
  libraryDependencies ++= {
    if (`is 2.13+`(scalaVersion.value))
      Seq.empty
    else
      Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch))
  })

val macrodeclaration = libraryDependencies ++= {
  if (`is 3+`(scalaVersion.value))
    Seq.empty
  else
    Seq(scalaOrganization.value % "scala-reflect" % scalaVersion.value % CompileInternal)
}

val jsweakreferences = libraryDependencies +=
  "org.scala-js" %%% "scalajs-weakreferences" % "1.0.0" cross CrossVersion.for3Use2_13

val jsmacrotaskexecutor = libraryDependencies +=
  "org.scala-js" %%% "scala-js-macrotask-executor" % "1.1.1"

val jsjavasecurerandom = libraryDependencies +=
  "org.scala-js" %%% "scalajs-java-securerandom" % "1.0.0" cross CrossVersion.for3Use2_13

val scalatest = libraryDependencies +=
  "org.scalatest" %%% "scalatest" % "3.2.15" % TestInternal

val scribe = libraryDependencies +=
  "com.outr" %%% "scribe" % "3.10.6"

val retypecheck = libraryDependencies ++= {
  if (`is 3+`(scalaVersion.value))
    Seq.empty
  else
    Seq("io.github.scala-loci" %% "retypecheck" % "0.10.0")
}

val rescala = libraryDependencies +=
  "de.tu-darmstadt.stg" %%% "rescala" % "0.32.0"

val upickle = libraryDependencies +=
  "com.lihaoyi" %%% "upickle" % "2.0.0"

val circe = Seq(
  libraryDependencies ++= {
    if (`is 2.12+`(scalaVersion.value))
      Seq(
        "io.circe" %%% "circe-core" % "0.14.1",
        "io.circe" %%% "circe-parser" % "0.14.1")
    else
      Seq.empty
  },
  compile / skip := (compile / skip).value || !`is 2.12+`(scalaVersion.value),
  publish / skip := (publish / skip).value || !`is 2.12+`(scalaVersion.value))

val jsoniter = Seq(
  libraryDependencies ++= {
    if (`is 2.12+`(scalaVersion.value))
      Seq("com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core" % "2.13.22")
    else
      Seq.empty
  },
  compile / skip := (compile / skip).value || !`is 2.12+`(scalaVersion.value),
  publish / skip := (publish / skip).value || !`is 2.12+`(scalaVersion.value))

val akkaHttp = libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % "10.1.15" % CompileInternal cross CrossVersion.for3Use2_13,
  "com.typesafe.akka" %% "akka-stream" % "2.5.32" % CompileInternal cross CrossVersion.for3Use2_13,
  "com.typesafe.akka" %% "akka-http" % "[10.0,11.0)" % Provided cross CrossVersion.for3Use2_13,
  "com.typesafe.akka" %% "akka-stream" % "[2.4,3.0)" % Provided cross CrossVersion.for3Use2_13)

val play = libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % "10.1.15" % CompileInternal cross CrossVersion.for3Use2_13,
  "com.typesafe.akka" %% "akka-stream" % "2.5.32" % CompileInternal cross CrossVersion.for3Use2_13,
  "com.typesafe.play" %% "play" % "2.7.9" % CompileInternal cross CrossVersion.for3Use2_13 withIsTransitive false,
  "com.typesafe.play" %% "play" % "[2.5,2.9)" % Provided cross CrossVersion.for3Use2_13 withIsTransitive false)

val scalajsDom = libraryDependencies +=
  "org.scala-js" % "scalajs-dom" % "2.3.0" cross ScalaJSCrossVersion.binary

val javalin = libraryDependencies +=
  "io.javalin" % "javalin" % "4.6.7"

val jetty = libraryDependencies ++= {
  val jettyVersion = "9.4.50.v20221201"
  Seq(
    "org.eclipse.jetty.websocket" % "websocket-server" % jettyVersion,
    "org.eclipse.jetty.websocket" % "websocket-client" % jettyVersion,
    "org.eclipse.jetty.websocket" % "websocket-api" % jettyVersion,
    "org.slf4j" % "slf4j-nop" % "2.0.6" % TestInternal)
}

val jetty11 = libraryDependencies ++= {
  val jettyVersion = "11.0.13"
  Seq(
    "org.eclipse.jetty.websocket" % "websocket-jetty-server" % jettyVersion exclude ("org.eclipse.jetty", "jetty-annotations"),
    "org.eclipse.jetty.websocket" % "websocket-jetty-client" % jettyVersion,
    "org.eclipse.jetty.websocket" % "websocket-jetty-api" % jettyVersion,
    "org.slf4j" % "slf4j-nop" % "2.0.6" % TestInternal)
}


lazy val loci = lociProject(
  project in file(".")
  settings (publish / skip := true,
            Global / onLoad := {
              val project = System.getenv("SCALA_PLATFORM") match {
                case "jvm" => Some("lociJVM")
                case "js" => Some("lociJS")
                case _ => None
              }
              val transformation = { state: State =>
                project map { project => s"project $project" :: state } getOrElse state
              }
              transformation compose (Global / onLoad).value
            })
  aggregate (lociJVM, lociJS))

lazy val lociJVM = lociProject(
  project in file(".jvm")
  settings (publish / skip := true,
            build := taskSequence(Compile / compile, Test / compile).value)
  aggregate (lociLanguageJVM, lociLanguageRuntimeJVM, lociCommunicationJVM,
             lociSerializerUpickleJVM,
             lociSerializerCirceJVM,
             lociSerializerJsoniterScalaJVM,
             lociTransmitterRescalaJVM, lociLanguageTransmitterRescalaJVM,
             lociCommunicatorTcpJVM,
             lociCommunicatorWsWebNativeJVM,
             lociCommunicatorWsAkkaJVM,
             lociCommunicatorWsAkkaPlayJVM,
             lociCommunicatorWsJavalinJVM,
             lociCommunicatorWsJettyJVM,
             lociCommunicatorWsJetty11JVM,
             lociCommunicatorWebRtcJVM))

lazy val lociJS = lociProject(
  project in file(".js")
  settings (publish / skip := true,
            build := taskSequence(Compile / compile, Test / compile,
                                  Compile / fastLinkJS, Test / fastLinkJS).value)
  aggregate (lociLanguageJS, lociLanguageRuntimeJS, lociCommunicationJS,
             lociSerializerUpickleJS,
             lociSerializerCirceJS,
             lociSerializerJsoniterScalaJS,
             lociTransmitterRescalaJS, lociLanguageTransmitterRescalaJS,
             lociCommunicatorTcpJS,
             lociCommunicatorWsWebNativeJS,
             lociCommunicatorBroadcastChannelJS,
             lociCommunicatorWsAkkaJS,
             lociCommunicatorWsAkkaPlayJS,
             lociCommunicatorWsJavalinJS,
             lociCommunicatorWsJettyJS,
             lociCommunicatorWsJetty11JS,
             lociCommunicatorWebRtcJS))


lazy val lociLanguage = lociProject.scala2only(
  name = "language",
  project = crossProject(JSPlatform, JVMPlatform)
    crossType CrossType.Full
    settings (retypecheck, macroparadise, macrodeclaration, scalatest),
  dependsOn = lociLanguageRuntime)

lazy val lociLanguageJVM = lociLanguage.jvm
lazy val lociLanguageJS = lociLanguage.js


lazy val lociLanguageRuntime = lociProject.scala2only(
  name = "language runtime",
  project = crossProject(JSPlatform, JVMPlatform)
    crossType CrossType.Pure
    settings (SourceGenerator.remoteSelection,
              retypecheck, macrodeclaration, scalatest)
    jsSettings jsweakreferences,
  dependsOn = lociCommunication)

lazy val lociLanguageRuntimeJVM = lociLanguageRuntime.jvm
lazy val lociLanguageRuntimeJS = lociLanguageRuntime.js


lazy val lociCommunicationPrelude = lociProject(
  crossProject(JSPlatform, JVMPlatform)
  crossType CrossType.Full
  in file("communication") / ".prelude"
  settings (normalizedName := "scala-loci-communication-prelude",
            publish / skip := true,
            Compile / unmanagedSourceDirectories := (Compile / unmanagedSourceDirectories).value flatMap {
              rebaseFile(_,
                (ThisBuild / baseDirectory).value / "communication" / ".prelude",
                (ThisBuild / baseDirectory).value / "communication")
            },
            Test / unmanagedSourceDirectories := (Test / unmanagedSourceDirectories).value flatMap {
              rebaseFile(_,
                (ThisBuild / baseDirectory).value / "communication" / ".prelude",
                (ThisBuild / baseDirectory).value / "communication")
            },
            Compile / unmanagedSources / includeFilter :=
              "ReflectionExtensions.scala" || "CompileTimeUtils.scala" || "SelectorResolution.scala",
            macrodeclaration, scalatest))

lazy val lociCommunicationPreludeJVM = lociCommunicationPrelude.jvm
lazy val lociCommunicationPreludeJS = lociCommunicationPrelude.js


lazy val lociCommunication = lociProject(
  name = "Communication",
  project = crossProject(JSPlatform, JVMPlatform)
    crossType CrossType.Full
    settings (Compile / unmanagedSources / excludeFilter :=
                "ReflectionExtensions.scala" || "CompileTimeUtils.scala" || "SelectorResolution.scala",
              SourceGenerator.transmittableTuples,
              SourceGenerator.functionsBindingBuilder,
              SourceGenerator.functionSubjectiveBinding,
              macroparadise, macrodeclaration, scribe, scalatest)
    jvmSettings copyCompiledFilesFrom(lociCommunicationPreludeJVM)
    jsSettings (copyCompiledFilesFrom(lociCommunicationPreludeJS),
                jsmacrotaskexecutor, jsjavasecurerandom))

lazy val lociCommunicationJVM = lociCommunication.jvm
lazy val lociCommunicationJS = lociCommunication.js


lazy val lociSerializerUpickle = lociProject(
  name = "µPickle serializer",
  file = "serializer-upickle",
  project = crossProject(JSPlatform, JVMPlatform)
    crossType CrossType.Pure
    settings upickle,
  dependsOn = lociCommunication)

lazy val lociSerializerUpickleJVM = lociSerializerUpickle.jvm
lazy val lociSerializerUpickleJS = lociSerializerUpickle.js


lazy val lociSerializerCirce = lociProject(
  name = "Circe serializer",
  file = "serializer-circe",
  project = crossProject(JSPlatform, JVMPlatform)
    crossType CrossType.Pure
    settings circe,
  dependsOn = lociCommunication)

lazy val lociSerializerCirceJVM = lociSerializerCirce.jvm
lazy val lociSerializerCirceJS = lociSerializerCirce.js


lazy val lociSerializerJsoniterScala = lociProject(
  name = "Jsoniter Scala serializer",
  file = "serializer-jsoniter-scala",
  project = crossProject(JSPlatform, JVMPlatform)
    crossType CrossType.Pure
    settings jsoniter,
  dependsOn = lociCommunication)

lazy val lociSerializerJsoniterScalaJVM = lociSerializerJsoniterScala.jvm
lazy val lociSerializerJsoniterScalaJS = lociSerializerJsoniterScala.js


lazy val lociTransmitterRescala = lociProject(
  name = "REScala transmitter",
  file = "transmitter-rescala",
  project = crossProject(JSPlatform, JVMPlatform)
    crossType CrossType.Pure
    settings rescala,
  dependsOn = lociCommunication)

lazy val lociTransmitterRescalaJVM = lociTransmitterRescala.jvm
lazy val lociTransmitterRescalaJS = lociTransmitterRescala.js


lazy val lociLanguageTransmitterRescala = lociProject.scala2only(
  name = "language REScala transmitter",
  file = "language-transmitter-rescala",
  project = crossProject(JSPlatform, JVMPlatform)
    crossType CrossType.Pure
    settings (macroparadise, macrodeclaration, scalatest),
  dependsOn = Projects(lociLanguage % TestInternal, lociLanguageRuntime, lociTransmitterRescala))

lazy val lociLanguageTransmitterRescalaJVM = lociLanguageTransmitterRescala.jvm
lazy val lociLanguageTransmitterRescalaJS = lociLanguageTransmitterRescala.js


lazy val lociCommunicatorTcp = lociProject(
  name = "TCP communicator",
  file = "communicator-tcp",
  project = crossProject(JSPlatform, JVMPlatform)
    crossType CrossType.Dummy
    settings scalatest,
  dependsOn = lociCommunication)

lazy val lociCommunicatorTcpJVM = lociCommunicatorTcp.jvm
lazy val lociCommunicatorTcpJS = lociCommunicatorTcp.js


lazy val lociCommunicatorWsWebNative = lociProject(
  name = "Web-native WebSocket communicator",
  file = "communicator-ws-webnative",
  project = crossProject(JSPlatform, JVMPlatform)
    crossType CrossType.Dummy
    settings scalajsDom,
  dependsOn = lociCommunication)

lazy val lociCommunicatorWsWebNativeJVM = lociCommunicatorWsWebNative.jvm
lazy val lociCommunicatorWsWebNativeJS = lociCommunicatorWsWebNative.js

lazy val lociCommunicatorBroadcastChannel = lociProject(
  name = "BroadcastChannel communicator",
  file = "communicator-broadcastchannel",
  project = crossProject(JSPlatform)
    crossType CrossType.Dummy
    settings scalajsDom,
  dependsOn = lociCommunication)

lazy val lociCommunicatorBroadcastChannelJS = lociCommunicatorBroadcastChannel.js

lazy val lociCommunicatorWsAkka = lociProject(
  name = "Akka WebSocket communicator",
  file = "communicator-ws-akka",
  project = crossProject(JSPlatform, JVMPlatform)
    crossType CrossType.Dummy
    settings (akkaHttp, scalatest),
  dependsOn = lociCommunication)

lazy val lociCommunicatorWsAkkaJVM = lociCommunicatorWsAkka.jvm
lazy val lociCommunicatorWsAkkaJS = lociCommunicatorWsAkka.js


lazy val lociCommunicatorWsAkkaPlay = lociProject(
  name = "Play Framework Akka WebSocket communicator",
  file = "communicator-ws-akka-play",
  project = crossProject(JSPlatform, JVMPlatform)
    crossType CrossType.Dummy
    settings play,
  dependsOn = lociCommunicatorWsAkka)

lazy val lociCommunicatorWsAkkaPlayJVM = lociCommunicatorWsAkkaPlay.jvm
lazy val lociCommunicatorWsAkkaPlayJS = lociCommunicatorWsAkkaPlay.js


lazy val lociCommunicatorWsJetty = lociProject(
  name = "Jetty WebSocket communicator",
  file = "communicator-ws-jetty",
  project = crossProject(JSPlatform, JVMPlatform)
    crossType CrossType.Dummy
    settings (jetty, scalatest),
  dependsOn = lociCommunication)

lazy val lociCommunicatorWsJettyJVM = lociCommunicatorWsJetty.jvm
lazy val lociCommunicatorWsJettyJS = lociCommunicatorWsJetty.js


lazy val lociCommunicatorWsJetty11 = lociProject(
  name = "Jetty 11 WebSocket communicator",
  file = "communicator-ws-jetty11",
  project = crossProject(JSPlatform, JVMPlatform)
            crossType CrossType.Dummy
            settings (jetty11, scalatest),
  dependsOn = lociCommunication)

lazy val lociCommunicatorWsJetty11JVM = lociCommunicatorWsJetty11.jvm
lazy val lociCommunicatorWsJetty11JS = lociCommunicatorWsJetty11.js


lazy val lociCommunicatorWsJavalin = lociProject(
  name = "Javalin WebSocket communicator",
  file = "communicator-ws-javalin",
  project = crossProject(JSPlatform, JVMPlatform)
    crossType CrossType.Dummy
    jvmSettings javalin,
  dependsOn = lociCommunication)

lazy val lociCommunicatorWsJavalinJVM = lociCommunicatorWsJavalin.jvm
lazy val lociCommunicatorWsJavalinJS = lociCommunicatorWsJavalin.js


lazy val lociCommunicatorWebRtc = lociProject(
  name = "WebRTC communicator",
  file = "communicator-webrtc",
  project = crossProject(JSPlatform, JVMPlatform)
    crossType CrossType.Full
    settings scalajsDom,
  dependsOn = lociCommunication)

lazy val lociCommunicatorWebRtcJVM = lociCommunicatorWebRtc.jvm
lazy val lociCommunicatorWebRtcJS = lociCommunicatorWebRtc.js
