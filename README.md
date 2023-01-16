# [ScalaLoci](https://scala-loci.github.io)


[![Build](https://img.shields.io/github/actions/workflow/status/scala-loci/scala-loci/build.yml?branch=master&label=build&logo=GitHub&logoColor=lightgray&style=for-the-badge)](https://github.com/scala-loci/scala-loci/actions/workflows/build.yml)
[![License](https://img.shields.io/github/license/scala-loci/scala-loci?label=license&logo=Open%20Source%20Initiative&logoColor=silver&style=for-the-badge)](https://github.com/scala-loci/scala-loci/blob/master/LICENSE)
[![Release](https://img.shields.io/maven-central/v/io.github.scala-loci/scala-loci-language_2.13?label=release&logo=Apache%20Maven&logoColor=lightgray&color=blue&style=for-the-badge)](https://search.maven.org/search?q=g:io.github.scala-loci%20a:scala-loci*)


ScalaLoci is a distributed programming language embedded into Scala.
The language provides a coherent model based on placement types that enables
reasoning about distributed data flows, supporting multiple software
architectures via dedicated language features and abstracting over low-level
communication details and data conversions. ScalaLoci simplifies developing
distributed systems, reduces error-prone communication code and favors early
detection of bugs.


## Getting ScalaLoci

1. Enable support for macro annotations in your `build.sbt`:

   * for Scala 2.13

     ```scala
     scalacOptions += "-Ymacro-annotations"
     ```

   * for Scala 2.11 or 2.12 ([Macro Paradise Plugin](https://docs.scala-lang.org/overviews/macros/paradise.html))

     ```scala
     addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch)
     ```

2. Add the ScalaLoci dependencies that you need for your system to your `build.sbt`:

   1. ScalaLoci language (always required)

      ```scala
      libraryDependencies ++= Seq(
        "io.github.scala-loci" %% "scala-loci-language" % "0.5.0" % "compile-internal",
        "io.github.scala-loci" %% "scala-loci-language-runtime" % "0.5.0")
      ```

   2. Transmitter for the types of values to be accessed remotely
      (built-in Scala types and standard collections are directly supported without additional dependencies)

      * [REScala](https://www.rescala-lang.com/) reactive events and signals

        ```scala
        libraryDependencies +=
          "io.github.scala-loci" %% "scala-loci-language-transmitter-rescala" % "0.5.0"
        ```

   3. Network communicators to connect the different components of the distributed system

      * TCP [*JVM only*]
  
        ```scala
        libraryDependencies +=
          "io.github.scala-loci" %% "scala-loci-communicator-tcp" % "0.5.0"
        ```

      * WebSocket (using web browser APIs) [*JS only, client only*]

        ```scala
        libraryDependencies +=
          "io.github.scala-loci" %% "scala-loci-communicator-ws-webnative" % "0.5.0"
        ```

      * WebSocket (using [Akka HTTP](https://doc.akka.io/docs/akka-http/)) [*JVM only*]

        ```scala
        libraryDependencies +=
          "io.github.scala-loci" %% "scala-loci-communicator-ws-akka" % "0.5.0"
        ```

      * WebSocket ([Play](https://www.playframework.com) integration using [Akka HTTP](https://doc.akka.io/docs/akka-http/)) [*JVM only*]

        ```scala
        libraryDependencies +=
          "io.github.scala-loci" %% "scala-loci-communicator-ws-akka-play" % "0.5.0"
        ```

      * WebSocket (using [Javalin](https://javalin.io)) [*JVM only, server only*]

        ```scala
        libraryDependencies +=
          "io.github.scala-loci" %% "scala-loci-communicator-ws-javalin" % "0.5.0"
        ```

      * WebSocket (using [Jetty](https://www.eclipse.org/jetty/)) [*JVM only*]

        ```scala
        libraryDependencies +=
          "io.github.scala-loci" %% "scala-loci-communicator-ws-jetty" % "0.5.0"
        ```

      * WebRTC (using web browser APIs) [*JS only*]

        ```scala
        libraryDependencies +=
          "io.github.scala-loci" %% "scala-loci-communicator-webrtc" % "0.5.0"
        ```

   4. Serializer for network communication

      * [µPickle](https://com-lihaoyi.github.io/upickle/) serialization

        ```scala
        libraryDependencies +=
          "io.github.scala-loci" %% "scala-loci-serializer-upickle" % "0.5.0"
        ```

      * [Circe](https://circe.github.io/circe/) serialization

        ```scala
        libraryDependencies +=
          "io.github.scala-loci" %% "scala-loci-serializer-circe" % "0.5.0"
        ```

      * [Jsoniter Scala](https://github.com/plokhotnyuk/jsoniter-scala/) serialization

        ```scala
        libraryDependencies +=
          "io.github.scala-loci" %% "scala-loci-serializer-jsoniter-scala" % "0.5.0"
        ```


## Using ScalaLoci network communication as a library

ScalaLoci’s underlying network communication library abstracting over different
network protocols can be used directly without ScalaLoci’s language
abstractions. The library provides pluggable *communicators* for different
network protocols and *transmitters* implementing the remote transmission
semantics for values of different types.

Add the ScalaLoci dependencies that you need for your system to your `build.sbt`:

1. ScalaLoci communication library (always required)

   ```scala
   libraryDependencies +=
     "io.github.scala-loci" %% "scala-loci-communication" % "0.5.0"
   ```

2. Transmitter for the types of values to be accessed remotely
   (built-in Scala types and standard collections are directly supported without additional dependencies)

   * [REScala](https://www.rescala-lang.com/) reactive events and signals

     ```scala
     libraryDependencies +=
       "io.github.scala-loci" %% "scala-loci-transmitter-rescala" % "0.5.0"
     ```

3. Network communicators to connect the different components of the distributed system (same as [above](#getting-scalaloci))

4. Serializer for network communication (same as [above](#getting-scalaloci))


## Examples and Case Studies

* Examples

  [Simple](https://github.com/scala-loci/examples-simple): Simple showcases of ScalaLoci using small examples

  [Variants](https://github.com/scala-loci/examples-variants): More complete examples implemented in different variants

* Case Studies

  [Flink](https://github.com/scala-loci/case-study-flink): Reimplementation of the task distribution system of [Apache Flink](https://flink.apache.org) in ScalaLoci

  [Gearpump](https://github.com/scala-loci/case-study-gearpump): Reimplementation of the worker assignment logic of [Apache Gearpump](https://gearpump.github.io/) in ScalaLoci

  [Play Scala.js](https://github.com/scala-loci/case-study-play-scalajs): Reimplementation of the [Play Framework with Scala.js Showcase](https://github.com/hussachai/play-scalajs-showcase) in ScalaLoci
