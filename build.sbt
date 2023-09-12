enablePlugins(GitVersioning)

Global / excludeLintKeys += git.useGitDescribe

ThisBuild / git.useGitDescribe := true

ThisBuild / organization := "io.github.scala-loci"

ThisBuild / homepage := Some(url("https://scala-loci.github.io/"))

ThisBuild / licenses += "Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")

val defaultSettings = Seq(
  scalacOptions ++= {
    if (`is 3+`(scalaVersion.value))
      Seq("-feature", "-deprecation", "-unchecked")
    else
      Seq("-feature", "-deprecation", "-unchecked", "-Xlint", "-language:higherKinds")
  },
)

ThisBuild / crossScalaVersions := Seq("2.11.12", "2.12.18", "2.13.12", "3.3.1")

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
  scalacOptions ++=
    (only { version => `is 2.13+`(version) && !`is 3+`(version) } orEmpty Def.setting {
      "-Ymacro-annotations"
    }).value,
  libraryDependencies ++=
    (only !(`is 2.13+`) orEmpty Def.setting {
      compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch)
    }).value)

val macrodeclaration = libraryDependencies ++= (only !(`is 3+`) orEmpty Def.setting {
  scalaOrganization.value % "scala-reflect" % scalaVersion.value % CompileInternal
}).value

val jsweakreferences = libraryDependencies +=
  "org.scala-js" %%% "scalajs-weakreferences" % "1.0.0" cross CrossVersion.for3Use2_13

val jsmacrotaskexecutor = libraryDependencies +=
  "org.scala-js" %%% "scala-js-macrotask-executor" % "1.1.1"

val jsjavasecurerandom = libraryDependencies +=
  "org.scala-js" %%% "scalajs-java-securerandom" % "1.0.0" cross CrossVersion.for3Use2_13

val scalatest = libraryDependencies +=
  "org.scalatest" %%% "scalatest" % "3.2.17" % TestInternal

// 3.10.7 is the latest version to support scalajs 1.12
val scribe = libraryDependencies +=
  "com.outr" %%% "scribe" % "3.10.7"

val retypecheck = libraryDependencies ++= (only !(`is 3+`) orEmpty Def.setting {
  "io.github.scala-loci" %% "retypecheck" % "0.10.0"
}).value

// 0.33.0 is the last one supporting Scala 2.11-2.13
val rescala = libraryDependencies +=
  "de.tu-darmstadt.stg" %%% "rescala" % "0.33.0"

val upickle = libraryDependencies +=
  "com.lihaoyi" %%% "upickle" % "2.0.0"

val circe = Seq(
  libraryDependencies ++= (only (`is 2.12+`) orEmpty Def.setting {
    Seq(
      "io.circe" %%% "circe-core" % "0.14.1",
      "io.circe" %%% "circe-parser" % "0.14.1")
  }).value
)

val jsoniter = Seq(
  libraryDependencies ++= (only (`is 2.12+`) orEmpty Def.setting {
    "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core" % "2.13.22"
  }).value
)

val akkaHttp = libraryDependencies ++= { (only.jvm orPlatformCompileTimeStubs Def.setting {
  Seq(
    "com.typesafe.akka" %% "akka-http" % "10.1.15" % CompileInternal cross CrossVersion.for3Use2_13,
    "com.typesafe.akka" %% "akka-stream" % "2.5.32" % CompileInternal cross CrossVersion.for3Use2_13,
    "com.typesafe.akka" %% "akka-http" % "10.1.15" % TestInternal cross CrossVersion.for3Use2_13,
    "com.typesafe.akka" %% "akka-stream" % "2.5.32" % TestInternal cross CrossVersion.for3Use2_13,
    "com.typesafe.akka" %% "akka-http" % "[10.0,11.0)" % Provided cross CrossVersion.for3Use2_13,
    "com.typesafe.akka" %% "akka-stream" % "[2.4,3.0)" % Provided cross CrossVersion.for3Use2_13)
}).value }

val play = libraryDependencies ++= (only.jvm orPlatformCompileTimeStubs Def.setting {
  Seq(
    "com.typesafe.akka" %% "akka-http" % "10.1.15" % CompileInternal cross CrossVersion.for3Use2_13,
    "com.typesafe.akka" %% "akka-stream" % "2.5.32" % CompileInternal cross CrossVersion.for3Use2_13,
    "com.typesafe.akka" %% "akka-http" % "10.1.15" % TestInternal cross CrossVersion.for3Use2_13,
    "com.typesafe.akka" %% "akka-stream" % "2.5.32" % TestInternal cross CrossVersion.for3Use2_13,
    "com.typesafe.play" %% "play" % "2.7.9" % CompileInternal cross CrossVersion.for3Use2_13 withIsTransitive false,
    "com.typesafe.play" %% "play" % "2.7.9" % TestInternal cross CrossVersion.for3Use2_13 withIsTransitive false,
    "com.typesafe.play" %% "play" % "[2.5,2.9)" % Provided cross CrossVersion.for3Use2_13 withIsTransitive false)
}).value

val scalajsDom = libraryDependencies ++= (only.js orPlatformCompileTimeStubs Def.setting {
  "org.scala-js" % "scalajs-dom" % "2.8.0" cross ScalaJSCrossVersion.binary
}).value

val javalin = libraryDependencies ++= (only.jvm orPlatformCompileTimeStubs Def.setting {
  "io.javalin" % "javalin" % "4.6.8"
}).value

val jetty = libraryDependencies ++= (only.jvm orPlatformCompileTimeStubs Def.setting {
  val jettyVersion = "9.4.53.v20231009"
  Seq(
    "org.eclipse.jetty.websocket" % "websocket-server" % jettyVersion,
    "org.eclipse.jetty.websocket" % "websocket-client" % jettyVersion,
    "org.eclipse.jetty.websocket" % "websocket-api" % jettyVersion,
    // "com.outr"  %% "scribe-slf4j"  % "3.10.7" % TestInternal
    "org.slf4j" % "slf4j-nop" % "2.0.11" % TestInternal)
}).value

val jetty12 = Seq(
  libraryDependencies ++= (only.jvm (`is 2.12+`) orPlatformCompileTimeStubs Def.setting {
    val jettyVersion = "12.0.5"
    Seq(
      "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-server" % jettyVersion,
      "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-client" % jettyVersion,
      "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-api" % jettyVersion,
      // "com.outr"  %% "scribe-slf4j2"  % "3.10.7" % TestInternal
      "org.slf4j" % "slf4j-nop" % "2.0.11" % TestInternal)
  }).value
)


lazy val loci = lociProject(
  project in file(".")
  settings (defaultSettings,
            publish / skip := true,
            crossScalaVersions := Nil,
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
  settings (defaultSettings,
            publish / skip := true,
            crossScalaVersions := Nil,
            build := taskSequence(Compile / compile, Test / compile).value)
  aggregate (lociLanguageJVM, lociLanguageRuntimeJVM, lociCommunicationJVM,
             lociSerializerUpickleJVM,
             lociSerializerCirceJVM,
             lociSerializerJsoniterScalaJVM,
             lociTransmitterRescalaJVM, lociLanguageTransmitterRescalaJVM,
             lociCommunicatorTcpJVM,
             lociCommunicatorWsWebNativeJVM,
             lociCommunicatorBroadcastChannelJVM,
             lociCommunicatorWsAkkaJVM,
             lociCommunicatorWsAkkaPlayJVM,
             lociCommunicatorWsJavalinJVM,
             lociCommunicatorWsJettyJVM,
             // including jetty 12 by default kills the 2.11 tests … but publication works …
             //lociCommunicatorWsJetty12JVM,
             lociCommunicatorWebRtcJVM))

lazy val lociJS = lociProject(
  project in file(".js")
  settings (defaultSettings,
            publish / skip := true,
            crossScalaVersions := Nil,
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
             // including jetty 12 by default kills the 2.11 tests … but publication works …
             // lociCommunicatorWsJetty12JS,
             lociCommunicatorWebRtcJS))


lazy val lociLanguage = lociProject.scala2only(
  name = "language",
  project = crossProject(JSPlatform, JVMPlatform)
    crossType CrossType.Full
    settings (defaultSettings,
              Test / fullClasspath := {
                (Test / fullClasspath).value filterNot { _.data == (Compile / classDirectory).value }
              },
              retypecheck, macroparadise, macrodeclaration, scalatest),
  dependsOn = lociLanguageRuntime)

lazy val lociLanguageJVM = lociLanguage.jvm
lazy val lociLanguageJS = lociLanguage.js


lazy val lociLanguageRuntime = lociProject.scala2only(
  name = "language runtime",
  project = crossProject(JSPlatform, JVMPlatform)
    crossType CrossType.Pure
    settings (defaultSettings,
              SourceGenerator.remoteSelection,
              retypecheck, macrodeclaration, scalatest)
    jsSettings jsweakreferences,
  dependsOn = lociCommunication)

lazy val lociLanguageRuntimeJVM = lociLanguageRuntime.jvm
lazy val lociLanguageRuntimeJS = lociLanguageRuntime.js


lazy val lociCommunicationPrelude = lociProject(
  crossProject(JSPlatform, JVMPlatform)
  crossType CrossType.Full
  in file("communication") / ".prelude"
  settings (defaultSettings,
            normalizedName := "scala-loci-communication-prelude",
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
    settings (defaultSettings,
              Compile / unmanagedSources / excludeFilter :=
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
    settings (defaultSettings, upickle),
  dependsOn = lociCommunication)

lazy val lociSerializerUpickleJVM = lociSerializerUpickle.jvm
lazy val lociSerializerUpickleJS = lociSerializerUpickle.js


lazy val lociSerializerCirce = lociProject.`scala 2.12+`(
  name = "Circe serializer",
  file = "serializer-circe",
  project = crossProject(JSPlatform, JVMPlatform)
    crossType CrossType.Pure
    settings (defaultSettings, circe),
  dependsOn = lociCommunication)

lazy val lociSerializerCirceJVM = lociSerializerCirce.jvm
lazy val lociSerializerCirceJS = lociSerializerCirce.js


lazy val lociSerializerJsoniterScala = lociProject.`scala 2.12+`(
  name = "Jsoniter Scala serializer",
  file = "serializer-jsoniter-scala",
  project = crossProject(JSPlatform, JVMPlatform)
    crossType CrossType.Pure
    settings (defaultSettings, jsoniter),
  dependsOn = lociCommunication)

lazy val lociSerializerJsoniterScalaJVM = lociSerializerJsoniterScala.jvm
lazy val lociSerializerJsoniterScalaJS = lociSerializerJsoniterScala.js


lazy val lociTransmitterRescala = lociProject(
  name = "REScala transmitter",
  file = "transmitter-rescala",
  project = crossProject(JSPlatform, JVMPlatform)
    crossType CrossType.Pure
    settings (defaultSettings, rescala),
  dependsOn = lociCommunication)

lazy val lociTransmitterRescalaJVM = lociTransmitterRescala.jvm
lazy val lociTransmitterRescalaJS = lociTransmitterRescala.js


lazy val lociLanguageTransmitterRescala = lociProject.scala2only(
  name = "language REScala transmitter",
  file = "language-transmitter-rescala",
  project = crossProject(JSPlatform, JVMPlatform)
    crossType CrossType.Pure
    settings (defaultSettings, macroparadise, macrodeclaration, scalatest),
  dependsOn = Projects(lociLanguage % TestInternal, lociLanguageRuntime, lociTransmitterRescala))

lazy val lociLanguageTransmitterRescalaJVM = lociLanguageTransmitterRescala.jvm
lazy val lociLanguageTransmitterRescalaJS = lociLanguageTransmitterRescala.js


lazy val lociCommunicatorTcp = lociProject(
  name = "TCP communicator",
  file = "communicator-tcp",
  project = crossProject(JSPlatform, JVMPlatform)
    crossType CrossType.Dummy
    settings (defaultSettings, scalatest),
  dependsOn = lociCommunication)

lazy val lociCommunicatorTcpJVM = lociCommunicatorTcp.jvm
lazy val lociCommunicatorTcpJS = lociCommunicatorTcp.js


lazy val lociCommunicatorWsWebNative = lociProject(
  name = "Web-native WebSocket communicator",
  file = "communicator-ws-webnative",
  project = crossProject(JSPlatform, JVMPlatform)
    crossType CrossType.Dummy
    settings (defaultSettings, scalajsDom),
  dependsOn = lociCommunication)

lazy val lociCommunicatorWsWebNativeJVM = lociCommunicatorWsWebNative.jvm
lazy val lociCommunicatorWsWebNativeJS = lociCommunicatorWsWebNative.js


lazy val lociCommunicatorBroadcastChannel = lociProject(
  name = "BroadcastChannel communicator",
  file = "communicator-broadcastchannel",
  project = crossProject(JSPlatform, JVMPlatform)
    crossType CrossType.Dummy
    settings (defaultSettings, scalajsDom),
  dependsOn = lociCommunication)

lazy val lociCommunicatorBroadcastChannelJVM = lociCommunicatorBroadcastChannel.jvm
lazy val lociCommunicatorBroadcastChannelJS = lociCommunicatorBroadcastChannel.js


lazy val lociCommunicatorWsAkka = lociProject(
  name = "Akka WebSocket communicator",
  file = "communicator-ws-akka",
  project = crossProject(JSPlatform, JVMPlatform)
    crossType CrossType.Dummy
    settings (defaultSettings, akkaHttp, scalatest),
  dependsOn = lociCommunication)

lazy val lociCommunicatorWsAkkaJVM = lociCommunicatorWsAkka.jvm
lazy val lociCommunicatorWsAkkaJS = lociCommunicatorWsAkka.js


lazy val lociCommunicatorWsAkkaPlay = lociProject(
  name = "Play Framework Akka WebSocket communicator",
  file = "communicator-ws-akka-play",
  project = crossProject(JSPlatform, JVMPlatform)
    crossType CrossType.Dummy
    settings (defaultSettings, play),
  dependsOn = lociCommunicatorWsAkka)

lazy val lociCommunicatorWsAkkaPlayJVM = lociCommunicatorWsAkkaPlay.jvm
lazy val lociCommunicatorWsAkkaPlayJS = lociCommunicatorWsAkkaPlay.js


lazy val lociCommunicatorWsJetty = lociProject(
  name = "Jetty WebSocket communicator",
  file = "communicator-ws-jetty",
  project = crossProject(JSPlatform, JVMPlatform)
    crossType CrossType.Dummy
    settings (defaultSettings, jetty, scalatest),
  dependsOn = lociCommunication)

lazy val lociCommunicatorWsJettyJVM = lociCommunicatorWsJetty.jvm
lazy val lociCommunicatorWsJettyJS = lociCommunicatorWsJetty.js


lazy val lociCommunicatorWsJetty12 = lociProject.`scala 2.12+`(
  name = "Jetty 12 WebSocket communicator",
  file = "communicator-ws-jetty12",
  project = crossProject(JSPlatform, JVMPlatform)
            crossType CrossType.Dummy
            settings (defaultSettings, jetty12, scalatest),
  dependsOn = lociCommunication)

lazy val lociCommunicatorWsJetty12JVM = lociCommunicatorWsJetty12.jvm
lazy val lociCommunicatorWsJetty12JS = lociCommunicatorWsJetty12.js


lazy val lociCommunicatorWsJavalin = lociProject(
  name = "Javalin WebSocket communicator",
  file = "communicator-ws-javalin",
  project = crossProject(JSPlatform, JVMPlatform)
    crossType CrossType.Dummy
    settings defaultSettings
    jvmSettings javalin,
  dependsOn = lociCommunication)

lazy val lociCommunicatorWsJavalinJVM = lociCommunicatorWsJavalin.jvm
lazy val lociCommunicatorWsJavalinJS = lociCommunicatorWsJavalin.js


lazy val lociCommunicatorWebRtc = lociProject(
  name = "WebRTC communicator",
  file = "communicator-webrtc",
  project = crossProject(JSPlatform, JVMPlatform)
    crossType CrossType.Full
    settings (defaultSettings, scalajsDom),
  dependsOn = lociCommunication)

lazy val lociCommunicatorWebRtcJVM = lociCommunicatorWebRtc.jvm
lazy val lociCommunicatorWebRtcJS = lociCommunicatorWebRtc.js
