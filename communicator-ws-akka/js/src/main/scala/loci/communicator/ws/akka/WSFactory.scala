package loci
package communicator
package ws.akka

import scala.annotation.compileTimeOnly

@compileTimeOnly("Akka WebSocket communicator only available on the JVM")
trait WSSetupFactory extends ConnectionSetupFactory.Implementation[WS] {
    this: WS.type =>

  val schemes = Seq.empty[String]

  protected def properties(
      implicit props: ConnectionSetupFactory.Properties): Properties =
    WS.Properties()

  protected def listener(
      url: String, scheme: String, location: String, properties: Properties) =
    None

  protected def connector(
      url: String, scheme: String, location: String, properties: Properties) =
    None
}

@compileTimeOnly("Akka WebSocket communicator only available on the JVM")
trait WSSecureSetupFactory extends ConnectionSetupFactory.Implementation[WS.Secure] {
    this: WS.Secure.type =>

  type Properties = WS.Properties

  val schemes = Seq.empty[String]

  protected def properties(
      implicit props: ConnectionSetupFactory.Properties): Properties =
    WS.Properties()

  protected def listener(
      url: String, scheme: String, location: String, properties: Properties) =
    None

  protected def connector(
      url: String, scheme: String, location: String, properties: Properties) =
    None
}
