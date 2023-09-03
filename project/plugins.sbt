libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.36"

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")

// 1.12.0 is the last version of scalajs supporting scala 2.11
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.12.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.2")
