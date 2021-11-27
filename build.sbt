import org.scalajs.sbtplugin.ScalaJSCrossVersion
import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}

enablePlugins(GitVersioning)

Global / excludeLintKeys += git.useGitDescribe

ThisBuild / git.useGitDescribe := true

ThisBuild / organization := "io.github.scala-loci"

ThisBuild / homepage := Some(url("https://scala-loci.github.io/"))

ThisBuild / licenses += "Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")

ThisBuild / scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked", "-Xlint", "-language:higherKinds")

ThisBuild / resolvers += "jitpack" at "https://jitpack.io"

ThisBuild / scalaVersion := "2.13.7"

ThisBuild / crossScalaVersions := Seq("2.11.12", "2.12.15", "2.13.7")

def `is 2.12+`(scalaVersion: String): Boolean =
  CrossVersion.partialVersion(scalaVersion) collect { case (2, n) => n >= 12 } getOrElse false

def `is 2.13+`(scalaVersion: String): Boolean =
  CrossVersion.partialVersion(scalaVersion) collect { case (2, n) => n >= 13 } getOrElse false


val macroparadise = Seq(
  scalacOptions ++= {
    if (`is 2.13+`(scalaVersion.value))
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

val macrodeclaration = libraryDependencies +=
  scalaOrganization.value % "scala-reflect" % scalaVersion.value % Provided

val jsweakreferences = libraryDependencies += 
  "org.scala-js" %%% "scalajs-weakreferences" % "1.0.0"

val scalatest = libraryDependencies +=
  "org.scalatest" %%% "scalatest" % "3.2.10" % Test

val scribe = libraryDependencies += {
  if (`is 2.12+`(scalaVersion.value))
    "com.outr" %%% "scribe" % "3.6.3"
  else
    "com.outr" %%% "scribe" % "3.6.2"
}

val retypecheck = libraryDependencies +=
  "io.github.scala-loci" %% "retypecheck" % "0.9.0"

val rescala = libraryDependencies +=
  "com.github.rescala-lang.rescala" %%% "rescala" % "cbb7980"

val upickle = libraryDependencies +=
  "com.lihaoyi" %%% "upickle" % "1.4.2"

val circe = Seq(
  libraryDependencies ++= {
    if (`is 2.12+`(scalaVersion.value))
      Seq(
        "io.circe" %%% "circe-core" % "0.14.1",
        "io.circe" %%% "circe-parser" % "0.14.1")
    else
      Seq.empty
  },
  (compile / skip) := !`is 2.12+`(scalaVersion.value),
  (publish / skip) := !`is 2.12+`(scalaVersion.value))

val jsoniter = Seq(
  libraryDependencies ++= {
    if (`is 2.12+`(scalaVersion.value))
      Seq("com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core" % "2.12.0")
    else
      Seq.empty
  },
  (compile / skip) := !`is 2.12+`(scalaVersion.value),
  (publish / skip) := !`is 2.12+`(scalaVersion.value))

val akkaHttp = libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % "[10.0,11.0)" % Provided,
  "com.typesafe.akka" %% "akka-stream" % "[2.4,3.0)" % Provided)

val play = libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % "[10.0,11.0)" % Provided,
  "com.typesafe.play" %% "play" % "[2.5,2.8)" % Provided)

val scalajsDom = libraryDependencies +=
  "org.scala-js" % "scalajs-dom" % "2.0.0" cross ScalaJSCrossVersion.binary

val javalin = libraryDependencies +=
  "io.javalin" % "javalin" % "4.1.1"

val jetty = libraryDependencies ++= Seq(
  "org.eclipse.jetty.websocket" % "websocket-server" % "9.4.44.v20210927",
  "org.eclipse.jetty.websocket" % "websocket-client" % "9.4.44.v20210927",
  "org.eclipse.jetty.websocket" % "websocket-api" % "9.4.44.v20210927",
  "org.slf4j" % "slf4j-nop" % "1.7.30" % Test)


lazy val loci = (project
  in file(".")
  settings ((publish / skip) := true)
  aggregate (lociJVM, lociJS))

lazy val lociJVM = (project
  in file(".jvm")
  settings ((publish / skip) := true)
  aggregate (lociLangJVM, lociCommunicationJVM, lociArchitecturesBasicJVM,
             lociSerializerUpickleJVM,
             lociSerializerCirceJVM,
             lociSerializerJsoniterScalaJVM,
             lociTransmitterRescalaJVM, lociLangTransmitterRescalaJVM,
             lociCommunicatorTcpJVM,
             lociCommunicatorWsWebNativeJVM,
             lociCommunicatorWsAkkaJVM, lociCommunicatorWsAkkaPlayJVM,
             lociCommunicatorWsJavalinJVM, lociCommunicatorWsJettyJVM,
             lociCommunicatorWebRtcJVM))

lazy val lociJS = (project
  in file(".js")
  settings ((publish / skip) := true)
  aggregate (lociLangJS, lociCommunicationJS, lociArchitecturesBasicJS,
             lociSerializerUpickleJS,
             lociSerializerCirceJS,
             lociSerializerJsoniterScalaJS,
             lociTransmitterRescalaJS, lociLangTransmitterRescalaJS,
             lociCommunicatorTcpJS,
             lociCommunicatorWsWebNativeJS,
             lociCommunicatorWsAkkaJS, lociCommunicatorWsAkkaPlayJS,
             lociCommunicatorWsJavalinJS, lociCommunicatorWsJettyJS,
             lociCommunicatorWebRtcJS))


lazy val lociLang = (crossProject(JSPlatform, JVMPlatform)
  crossType CrossType.Full
  in file("scala-loci-lang")
  settings (normalizedName := "scala-loci-lang",
            SourceGenerator.remoteSelection,
            retypecheck, macroparadise, macrodeclaration, scribe, scalatest)
  jsSettings jsweakreferences
  dependsOn lociCommunication % "compile->compile;test->test")

lazy val lociLangJVM = lociLang.jvm
lazy val lociLangJS = lociLang.js


lazy val lociCommunication = (crossProject(JSPlatform, JVMPlatform)
  crossType CrossType.Full
  in file("scala-loci-communication")
  settings (normalizedName := "scala-loci-communication",
            Compile / unmanagedSourceDirectories +=
                (ThisBuild / baseDirectory).value / "scala-loci-communication" / "shared" / "src" / "test" / "scala",
            Compile / unmanagedSources / excludeFilter := {
              val testDirectory =
                (ThisBuild / baseDirectory).value / "scala-loci-communication" / "shared" / "src" / "test" / "scala"
              new SimpleFileFilter(file =>
                (file.getCanonicalPath startsWith testDirectory.getCanonicalPath) && !(file.getName startsWith "CompileTimeUtils"))
            },
            Compile / packageBin / mappings ~= { _ filter { case (file, _) => !(file.getName startsWith "CompileTimeUtils") } },
            Test / unmanagedSources / excludeFilter := NothingFilter,
            SourceGenerator.transmittableTuples,
            SourceGenerator.functionsBindingBuilder,
            SourceGenerator.functionSubjectiveBinding,
            macroparadise, macrodeclaration, scribe, scalatest))

lazy val lociCommunicationJVM = lociCommunication.jvm
lazy val lociCommunicationJS = lociCommunication.js


lazy val lociArchitecturesBasic = (crossProject(JSPlatform, JVMPlatform)
  crossType CrossType.Pure
  in file("scala-loci-architectures-basic")
  settings (normalizedName := "scala-loci-architectures-basic",
            macroparadise)
  dependsOn lociLang)

lazy val lociArchitecturesBasicJVM = lociArchitecturesBasic.jvm
lazy val lociArchitecturesBasicJS = lociArchitecturesBasic.js


lazy val lociSerializerUpickle = (crossProject(JSPlatform, JVMPlatform)
  crossType CrossType.Pure
  in file("scala-loci-serializer-upickle")
  settings (normalizedName := "scala-loci-serializer-upickle",
            upickle)
  dependsOn lociCommunication)

lazy val lociSerializerUpickleJVM = lociSerializerUpickle.jvm
lazy val lociSerializerUpickleJS = lociSerializerUpickle.js


lazy val lociSerializerCirce = (crossProject(JSPlatform, JVMPlatform)
  crossType CrossType.Pure
  in file("scala-loci-serializer-circe")
  settings (normalizedName := "scala-loci-serializer-circe",
            circe)
  dependsOn lociCommunication)

lazy val lociSerializerCirceJVM = lociSerializerCirce.jvm
lazy val lociSerializerCirceJS = lociSerializerCirce.js


lazy val lociSerializerJsoniterScala = (crossProject(JSPlatform, JVMPlatform)
  crossType CrossType.Pure
  in file("scala-loci-serializer-jsoniter-scala")
  settings (normalizedName := "scala-loci-serializer-jsoniter-scala",
            jsoniter)
  dependsOn lociCommunication)

lazy val lociSerializerJsoniterScalaJVM = lociSerializerJsoniterScala.jvm
lazy val lociSerializerJsoniterScalaJS = lociSerializerJsoniterScala.js


lazy val lociTransmitterRescala = (crossProject(JSPlatform, JVMPlatform)
  crossType CrossType.Pure
  in file("scala-loci-transmitter-rescala")
  settings (normalizedName := "scala-loci-transmitter-rescala",
            rescala, scalatest)
  dependsOn lociCommunication % "compile->compile;test->test")

lazy val lociTransmitterRescalaJVM = lociTransmitterRescala.jvm
lazy val lociTransmitterRescalaJS = lociTransmitterRescala.js


lazy val lociLangTransmitterRescala = (crossProject(JSPlatform, JVMPlatform)
  crossType CrossType.Pure
  in file("scala-loci-lang-transmitter-rescala")
  settings (normalizedName := "scala-loci-lang-transmitter-rescala",
            macroparadise, macrodeclaration, scalatest)
  dependsOn (lociLang % "compile->compile;test->test",
             lociTransmitterRescala % "compile->compile;test->test"))

lazy val lociLangTransmitterRescalaJVM = lociLangTransmitterRescala.jvm
lazy val lociLangTransmitterRescalaJS = lociLangTransmitterRescala.js


lazy val lociCommunicatorTcp = (crossProject(JSPlatform, JVMPlatform)
  crossType CrossType.Dummy
  in file("scala-loci-communicator-tcp")
  settings (normalizedName := "scala-loci-communicator-tcp",
            scalatest)
  dependsOn lociCommunication % "compile->compile;test->test")

lazy val lociCommunicatorTcpJVM = lociCommunicatorTcp.jvm
lazy val lociCommunicatorTcpJS = lociCommunicatorTcp.js


lazy val lociCommunicatorWsWebNative = (crossProject(JSPlatform, JVMPlatform)
  crossType CrossType.Dummy
  in file("scala-loci-communicator-ws-webnative")
  settings (normalizedName := "scala-loci-communicator-ws-webnative",
            akkaHttp, scalajsDom, scalatest)
  dependsOn lociCommunication)

lazy val lociCommunicatorWsWebNativeJVM = lociCommunicatorWsWebNative.jvm
lazy val lociCommunicatorWsWebNativeJS = lociCommunicatorWsWebNative.js


lazy val lociCommunicatorWsAkka = (crossProject(JSPlatform, JVMPlatform)
  crossType CrossType.Dummy
  in file("scala-loci-communicator-ws-akka")
  settings (normalizedName := "scala-loci-communicator-ws-akka",
            akkaHttp, scalajsDom, scalatest)
  dependsOn lociCommunication % "compile->compile;test->test")

lazy val lociCommunicatorWsAkkaJVM = lociCommunicatorWsAkka.jvm
lazy val lociCommunicatorWsAkkaJS = lociCommunicatorWsAkka.js


lazy val lociCommunicatorWsAkkaPlay = (crossProject(JSPlatform, JVMPlatform)
  crossType CrossType.Dummy
  in file("scala-loci-communicator-ws-akka-play")
  settings (normalizedName := "scala-loci-communicator-ws-akka-play",
            play)
  dependsOn lociCommunicatorWsAkka)

lazy val lociCommunicatorWsAkkaPlayJVM = lociCommunicatorWsAkkaPlay.jvm
lazy val lociCommunicatorWsAkkaPlayJS = lociCommunicatorWsAkkaPlay.js


lazy val lociCommunicatorWsJetty = (crossProject(JSPlatform, JVMPlatform)
  crossType CrossType.Dummy
  in file("scala-loci-communicator-ws-jetty")
  settings (normalizedName := "scala-loci-communicator-ws-jetty",
            jetty, scalatest)
  dependsOn lociCommunication % "compile->compile;test->test")

lazy val lociCommunicatorWsJettyJVM = lociCommunicatorWsJetty.jvm
lazy val lociCommunicatorWsJettyJS = lociCommunicatorWsJetty.js


lazy val lociCommunicatorWsJavalin = (crossProject(JSPlatform, JVMPlatform)
  crossType CrossType.Dummy
  in file("scala-loci-communicator-ws-javalin")
  settings (normalizedName := "scala-loci-communicator-ws-javalin",
            scalajsDom)
  jvmSettings javalin
  dependsOn lociCommunication)

lazy val lociCommunicatorWsJavalinJVM = lociCommunicatorWsJavalin.jvm
lazy val lociCommunicatorWsJavalinJS = lociCommunicatorWsJavalin.js


lazy val lociCommunicatorWebRtc = (crossProject(JSPlatform, JVMPlatform)
  crossType CrossType.Full
  in file("scala-loci-communicator-webrtc")
  settings (normalizedName := "scala-loci-communicator-webrtc",
            scalajsDom)
  dependsOn lociCommunication)

lazy val lociCommunicatorWebRtcJVM = lociCommunicatorWebRtc.jvm
lazy val lociCommunicatorWebRtcJS = lociCommunicatorWebRtc.js
