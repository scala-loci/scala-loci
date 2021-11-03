package loci
package communicator
package ws.jetty

import loci.communicator.ConnectionSetupFactory.Implementation

import scala.concurrent.duration.FiniteDuration

private object WSSetupParser extends
    ConnectionSetupParser with
    SimpleConnectionSetupProperties {

  type Properties = WS.Properties

  def properties(implicit props: ConnectionSetupFactory.Properties) =
    WS.Properties()
      .set[FiniteDuration]("heartbeat-delay") { v => _.copy(heartbeatDelay = v) }
      .set[FiniteDuration]("heartbeat-timeout") { v => _.copy(heartbeatTimeout = v) }
}

trait WSSetupFactory extends ConnectionSetupFactory.Implementation[WS] {
    this: WS.type =>

  val schemes = Seq("ws", "wss")

  protected def properties(
      implicit props: ConnectionSetupFactory.Properties): Properties =
    WSSetupParser.properties

  protected def listener(
      url: String, scheme: String, location: String, properties: Properties) =
    None

  protected def connector(
      url: String, scheme: String, location: String, properties: Properties) =
    Some(WS(url, properties))
}

trait WSSecureSetupFactory extends Implementation[WS.Secure] {
  this: WS.Secure.type =>

  type Properties = WS.Properties

  val schemes = Seq("wss")

  protected def properties(
      implicit props: ConnectionSetupFactory.Properties): Properties =
    WSSetupParser.properties

  protected def listener(
      url: String, scheme: String, location: String, properties: Properties) =
    None

  protected def connector(
      url: String, scheme: String, location: String, properties: Properties) =
    Some(WS.Secure(url, properties))
}

