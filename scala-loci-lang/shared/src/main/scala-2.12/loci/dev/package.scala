package loci

import loci.communicator._
import loci.dev.language._

import scala.annotation.{StaticAnnotation, compileTimeOnly, showAsInfix}
import scala.language.experimental.macros
import scala.language.higherKinds

package dev {
  @compileTimeOnly("enable macro paradise to use multitier code")
  final class multitier extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro language.impl.Multitier.annotation
  }

  object multitier {
    def start[P, Inst[P] <: Instance[P]](instance: Inst[P]): Instance[P] =
      macro language.impl.Instance.start
  }

  final class peer extends StaticAnnotation

  sealed trait Single[P] extends Multiple[P]
  sealed trait Optional[P] extends Multiple[P]
  sealed trait Multiple[+P]

  trait Remote[+P] extends Equals
  object Remote extends transmitter.Remote
}

package object dev {
  type Local[T] = T

  @showAsInfix type on[T, P] = Placed[T, P] with T
  @showAsInfix type per[T, P] = Placed.Subjective[T, P]

  def connect[P](setup: Connector[ProtocolCommon]): Connections =
    macro language.impl.Connections.setup
  def connect[P](factory: ConnectionSetupFactory[ProtocolCommon])(
      /* url: String, props: ConnectionSetupFactory.Properties */ args: Any*): Connections =
    macro language.impl.Connections.factory

  def listen[P](setup: Listener[ProtocolCommon]): Connections =
    macro language.impl.Connections.setup
  def listen[P](factory: ConnectionSetupFactory[ProtocolCommon])(
      /* url: String, props: ConnectionSetupFactory.Properties */ args: Any*): Connections =
    macro language.impl.Connections.factory

  def placed: Placement.Placed = erased
  def on: Placement.Select[Placement.Run] = erased
  def on[P]: Placement.On[P] with Placement.Run[P, language.from] = erased
  def remote: Placement.Narrow with Placement.Select[Placement.Call] with Placement.Call[Nothing, language.from] = erased
  def remote[P]: Placement.Call[P, language.from] = erased
}
