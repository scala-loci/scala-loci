package loci
package communicator
package ws.javalin

import scala.annotation.compileTimeOnly

@compileTimeOnly("Javalin WebSocket communicator only available on the JVM")
trait WSSetupFactory
    extends ConnectionSetupFactory.Implementation[WS]
    with ConnectionSetupParser
    with SimpleConnectionSetupProperties {
  val self: WS.type = unavailable

  val schemes = unavailable

  protected def properties(implicit props: ConnectionSetupFactory.Properties) = unavailable
  protected def listener(url: String, scheme: String, location: String, properties: WS.Properties) = unavailable
  protected def connector(url: String, scheme: String, location: String, properties: WS.Properties) = unavailable
}
