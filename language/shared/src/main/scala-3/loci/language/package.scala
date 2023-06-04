package loci
package language

import embedding.*

import scala.annotation.experimental

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
object on extends PlacedExpression.Select[PlacedExpression.Run]:
  sealed trait on[P] extends PlacedExpression.On.Fallback[P], PlacedExpression.Run[P, from]
  transparent inline def apply[P]: on[P] = ${ PlacedExpression.On[P] }

@experimental
object remote extends PlacedExpression.Narrow, PlacedExpression.Select[PlacedExpression.Call], PlacedExpression.Call[Nothing, from], Gateway[Nothing]:
  sealed trait remote[P] extends PlacedExpression.Call[P, from], Gateway[P]
  def apply[P]: remote[P] = erased
