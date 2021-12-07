package loci
package communicator
package ws.javalin

import scala.concurrent.duration.FiniteDuration

trait WSSetupFactory extends
  ConnectionSetupFactory.Implementation[WS] with
  ConnectionSetupParser with
  SimpleConnectionSetupProperties {
    this: WS.type =>

  val schemes = Seq.empty[String]

  protected def properties(implicit props: ConnectionSetupFactory.Properties) =
    Properties()
      .set[FiniteDuration]("heartbeat-delay") { v => _.copy(heartbeatDelay = v) }
      .set[FiniteDuration]("heartbeat-timeout") { v => _.copy(heartbeatTimeout = v) }

  protected def listener(
      url: String, scheme: String, location: String, properties: Properties) =
    None

  protected def connector(
      url: String, scheme: String, location: String, properties: Properties) =
    None
}
