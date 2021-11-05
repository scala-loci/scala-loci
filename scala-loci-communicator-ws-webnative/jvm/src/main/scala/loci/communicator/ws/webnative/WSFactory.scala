package loci
package communicator
package ws.webnative

import scala.annotation.compileTimeOnly

@compileTimeOnly("Web native WebSocket communicator only available in JS")
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

@compileTimeOnly("Web native WebSocket communicator only available in JS")
trait WSSecureSetupFactory extends ConnectionSetupFactory.Implementation[WS.Secure] {
    this: WS.Secure.type =>

  type Properties = WS.Properties

  val schemes = Seq("wss")

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
