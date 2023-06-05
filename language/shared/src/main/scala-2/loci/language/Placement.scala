package loci
package language

import communicator._
import embedding._
import messaging._

import scala.annotation.compileTimeOnly
import scala.language.experimental.macros

object connect {
  def apply[P](setup: Connector[ConnectionsBase.Protocol]): Connections =
    macro impl.Connections.setup
  def apply[P](factory: ConnectionSetupFactory[ConnectionsBase.Protocol])(
      /* url: String, props: ConnectionSetupFactory.Properties */ args: Any*): Connections =
    macro impl.Connections.factory
}

object listen {
  def apply[P](setup: Listener[ConnectionsBase.Protocol]): Connections =
    macro impl.Connections.setup
  def apply[P](factory: ConnectionSetupFactory[ConnectionsBase.Protocol])(
      /* url: String, props: ConnectionSetupFactory.Properties */ args: Any*): Connections =
    macro impl.Connections.factory
}

@compileTimeOnly("Placed expression can only used in multitier module")
object placed extends On.Placed

@compileTimeOnly("Placed expression can only used in multitier module")
object on extends Select[Run] {
  sealed trait on[P] extends On[P] with Run[P, from]
  def apply[P]: on[P] = erased
}

@compileTimeOnly("Placed expression can only used in multitier module")
object remote extends Narrow with Select[Call] with Call[Nothing, from] with Gateway[Nothing] {
  sealed trait remote[P] extends Call[P, from] with Gateway[P]
  def apply[P]: remote[P] = erased
}
