libraryDependencies += "org.slf4j" % "slf4j-nop" % "2.0.13"

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % (if (Option(System.getenv("SCALA_VERSION")) exists { _ startsWith "2.11" }) "1.12.0" else "1.16.0"))

addSbtPlugin("com.github.sbt" % "sbt-git" % "2.0.1")
