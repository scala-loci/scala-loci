package loci
package communicator
package ws.javalin

import scala.annotation.compileTimeOnly

@compileTimeOnly("Javalin WebSocket communicator only available on the JVM")
trait WSSetupFactory extends
  ConnectionSetupFactory.Implementation[WS] with
  ConnectionSetupParser with
  SimpleConnectionSetupProperties {
    this: WS.type =>

  val schemes = Seq.empty[String]

  protected def properties(implicit props: ConnectionSetupFactory.Properties) =
    Properties()

  protected def listener(
      url: String, scheme: String, location: String, properties: Properties) =
    None

  protected def connector(
      url: String, scheme: String, location: String, properties: Properties) =
    None
}
