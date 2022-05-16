package loci
package communicator
package ws.akka

import scala.annotation.compileTimeOnly

@compileTimeOnly("Akka WebSocket communicator only available on the JVM")
trait WSSetupFactory extends ConnectionSetupFactory.Implementation[WS] {
  val self: WS.type = unavailable

  val schemes = unavailable

  protected def properties(implicit props: ConnectionSetupFactory.Properties) = unavailable
  protected def listener(url: String, scheme: String, location: String, properties: WS.Properties) = unavailable
  protected def connector(url: String, scheme: String, location: String, properties: WS.Properties) = unavailable
}

@compileTimeOnly("Akka WebSocket communicator only available on the JVM")
trait WSSecureSetupFactory extends ConnectionSetupFactory.Implementation[WS.Secure] {
  val self: WS.type = unavailable

  val schemes = unavailable

  protected def properties(implicit props: ConnectionSetupFactory.Properties) = unavailable
  protected def listener(url: String, scheme: String, location: String, properties: WS.Properties) = unavailable
  protected def connector(url: String, scheme: String, location: String, properties: WS.Properties) = unavailable
}
