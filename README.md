# [ScalaLoci](http://scala-loci.github.io)

ScalaLoci is a distributed programming language embedded into Scala.
The language provides a coherent model based on placement types that enables
reasoning about distributed data flows, supporting multiple software
architectures via dedicated language features and abstracting over low-level
communication details and data conversions. ScalaLoci simplifies developing
distributed systems, reduces error-prone communication code and favors early
detection of bugs.


## Getting ScalaLoci

1. Enable the [Macro Paradise Plugin](http://docs.scala-lang.org/overviews/macros/paradise.html) (for macro annotations) in your `build.sbt`

   ```scala
   addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch)
   ```

2. Add the resolver for the ScalaLoci dependencies to your `build.sbt`

   ```scala
   resolvers += Resolver.bintrayRepo("stg-tud", "maven")
   ```

3. Add the ScalaLoci dependencies that you need for your system to your `build.sbt`

   1. ScalaLoci language (always required)

      ```scala
      libraryDependencies += "de.tuda.stg" %% "scala-loci-lang" % "0.2.0"
      ```

   2. Transmitter for the types of values to be accessed remotely

      * Built-in Scala types and standard collections

        ```scala
        libraryDependencies += "de.tuda.stg" %% "scala-loci-lang-transmitter-basic" % "0.2.0"
        ```

      * [REScala](http://www.rescala-lang.com/) reactive events and signals

        ```scala
        libraryDependencies += "de.tuda.stg" %% "scala-loci-lang-transmitter-rescala" % "0.2.0"
        ```

   3. Network communicators to connect the different components of the distributed system

      * TCP [*JVM only*]
  
        ```scala
        libraryDependencies += "de.tuda.stg" %% "scala-loci-communicator-tcp" % "0.2.0"
        ```

      * WebSocket (using [Akka HTTP](http://doc.akka.io/docs/akka-http/current/) on the JVM) [*server: JVM only, client: JVM and JS web browser APIs*]

        ```scala
        libraryDependencies += "de.tuda.stg" %% "scala-loci-communicator-ws-akka" % "0.2.0"
        ```

      * WebSocket ([Play](http://www.playframework.com) integration) [*server: JVM only, client: JVM and JS web browser APIs*]

        ```scala
        libraryDependencies += "de.tuda.stg" %% "scala-loci-communicator-ws-akka-play" % "0.2.0"
        ```

      * WebRTC [*JS web browser APIs only*]

        ```scala
        libraryDependencies += "de.tuda.stg" %% "scala-loci-communicator-webrtc" % "0.2.0"
        ```

   4. Serializer for network communication

      * [µPickle](http://www.lihaoyi.com/upickle/) serialization

        ```scala
        libraryDependencies += "de.tuda.stg" %% "scala-loci-serializer-upickle" % "0.2.0"
        ```

      * [Circe](http://circe.github.io/circe/) serialization

        ```scala
        libraryDependencies += "de.tuda.stg" %% "scala-loci-serializer-circe" % "0.2.0"
        ```


## Using ScalaLoci network communication as a library

ScalaLoci’s underlying network communication library abstracting over different
network protocols can be used directly without ScalaLoci’s language
abstractions. The library provides pluggable *communicators* for different
network protocols and *transmitters* implementing the remote transmission
semantics for values of different types.

1. Add the resolver for the ScalaLoci dependencies to your `build.sbt` (same as [above](#getting-scalaloci))

2. Add the ScalaLoci dependencies that you need for your system to your `build.sbt`

   1. ScalaLoci communication library (always required)

      ```scala
      libraryDependencies += "de.tuda.stg" %% "scala-loci-communication" % "0.2.0"
      ```

   2. Transmitter for the types of values to be accessed remotely

      * Built-in Scala types and standard collections

        ```scala
        libraryDependencies += "de.tuda.stg" %% "scala-loci-transmitter-basic" % "0.2.0"
        ```

      * [REScala](http://www.rescala-lang.com/) reactive events and signals

        ```scala
        libraryDependencies += "de.tuda.stg" %% "scala-loci-transmitter-rescala" % "0.2.0"
        ```

   3. Network communicators to connect the different components of the distributed system (same as [above](#getting-scalaloci))

   4. Serializer for network communication (same as [above](#getting-scalaloci))


## Examples and Case Studies

* Examples

  [Simple](http://github.com/scala-loci/examples-simple): Simple showcases of ScalaLoci using small examples

  [Variants](http://github.com/scala-loci/examples-variants): More complete examples implemented in different variants

* Case Studies

  [Flink](http://github.com/scala-loci/case-study-flink): Reimplementation of the task distribution system of [Apache Flink](http://flink.apache.org) in ScalaLoci

  [Gearpump](http://github.com/scala-loci/case-study-gearpump): Reimplementation of the worker assignment logic of [Apache Gearpump](http://gearpump.apache.org/) in ScalaLoci

  [Play Scala.js](http://github.com/scala-loci/case-study-play-scalajs): Reimplementation of the [Play Framework with Scala.js Showcase](http://github.com/hussachai/play-scalajs-showcase) in ScalaLoci
