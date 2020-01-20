package loci.communicator.ws.javalin

import loci.communicator.ConnectionSetupFactory.Implementation
import loci.communicator.{ConnectionSetupFactory, ConnectionSetupParser, SimpleConnectionSetupProperties}

import scala.concurrent.duration.FiniteDuration

trait WSSetupFactory extends
                     Implementation[WS] with
                     ConnectionSetupParser with
                     SimpleConnectionSetupProperties { this: WS.type =>
  val schemes = Seq()

  protected def properties(implicit props: ConnectionSetupFactory.Properties) =
    Properties()
      .set[FiniteDuration]("heartbeat-delay") { v => _.copy(heartbeatDelay = v) }
      .set[FiniteDuration]("heartbeat-timeout") { v => _.copy(heartbeatTimeout = v) }

  protected def listener(
      url: String, scheme: String, location: String, properties: Properties) = None

  protected def connector(
      url: String, scheme: String, location: String, properties: Properties) = None

}
