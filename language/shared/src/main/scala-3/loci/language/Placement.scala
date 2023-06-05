package loci
package language

import embedding.*

import scala.annotation.{compileTimeOnly, experimental}

//def connect[P](setup: Connector[ConnectionsBase.Protocol]): Connections =
//  macro impl.Connections.setup
//def connect[P](factory: ConnectionSetupFactory[ConnectionsBase.Protocol])(
//    /* url: String, props: ConnectionSetupFactory.Properties */ args: Any*): Connections =
//  macro impl.Connections.factory
//
//def listen[P](setup: Listener[ConnectionsBase.Protocol]): Connections =
//  macro impl.Connections.setup
//def listen[P](factory: ConnectionSetupFactory[ConnectionsBase.Protocol])(
//    /* url: String, props: ConnectionSetupFactory.Properties */ args: Any*): Connections =
//  macro impl.Connections.factory

@experimental
@compileTimeOnly("Placed expression can only used in multitier module")
object on extends Select[Run]:
  sealed trait on[P] extends On.Fallback[P], Run[P, from]
  transparent inline def apply[P]: on[P] = ${ On[P] }

@experimental
@compileTimeOnly("Placed expression can only used in multitier module")
object remote extends Narrow, Select[Call], Call[Nothing, from], Gateway[Nothing]:
  sealed trait remote[P] extends Call[P, from], Gateway[P]
  def apply[P]: remote[P] = erased
