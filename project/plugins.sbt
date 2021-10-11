libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.30"

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % (if ("0.6" == System.getenv("SCALAJS")) "0.6.32" else "1.7.0"))

addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.0")
