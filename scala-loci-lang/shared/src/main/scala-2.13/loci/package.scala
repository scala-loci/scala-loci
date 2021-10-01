import loci.communicator._
import loci.language._
import loci.messaging._

import scala.annotation.{StaticAnnotation, compileTimeOnly, showAsInfix}
import scala.language.experimental.macros

package loci {
  @compileTimeOnly("enable macro paradise to use multitier code")
  final class multitier extends StaticAnnotation {
    def this(accessorGeneration: AccessorGeneration) = this()
    def macroTransform(annottees: Any*): Any = macro impl.Multitier.annotation
  }

  object multitier {
    def start[P, Inst[P] <: Instance[P]](instance: Inst[P]): Runtime[P] =
      macro impl.Instance.start

    @compileTimeOnly("method can only be invoked in multitier code")
    def running: Boolean = erased

    @compileTimeOnly("method can only be invoked in multitier code")
    def terminate(): Unit = erased
  }

  final class peer extends StaticAnnotation

  sealed trait Single[P] extends Multiple[P]
  sealed trait Optional[P] extends Multiple[P]
  sealed trait Multiple[+P]

  trait Remote[+P] extends Equals
  object Remote extends transmitter.RemoteReference

  trait SelfReference[+P] extends Remote.Reference[P]
}

package object loci {
  type Local[T] = T

  @showAsInfix type on[T, P] = Placed[T, P] with T
  @showAsInfix type per[T, P] = Placed.Subjective[T, P]

  def connect[P](setup: Connector[ConnectionsBase.Protocol]): Connections =
    macro impl.Connections.setup
  def connect[P](factory: ConnectionSetupFactory[ConnectionsBase.Protocol])(
      /* url: String, props: ConnectionSetupFactory.Properties */ args: Any*): Connections =
    macro impl.Connections.factory

  def listen[P](setup: Listener[ConnectionsBase.Protocol]): Connections =
    macro impl.Connections.setup
  def listen[P](factory: ConnectionSetupFactory[ConnectionsBase.Protocol])(
      /* url: String, props: ConnectionSetupFactory.Properties */ args: Any*): Connections =
    macro impl.Connections.factory

  def placed: Placement.Placed = erased
  def on: Placement.Select[Placement.Run] = erased
  def on[P]: Placement.On[P] with Placement.Run[P, from] = erased
  def remote: Placement.Narrow with Placement.Select[Placement.Call] with Placement.Call[Nothing, from] with Gateway[Nothing] = erased
  def remote[P]: Placement.Call[P, from] with Gateway[P] = erased

  def onAny: Placement.SelectAny[Placement.Run] = erased
  def remoteAny: Placement.SelectAny[Placement.Call] = erased
}
