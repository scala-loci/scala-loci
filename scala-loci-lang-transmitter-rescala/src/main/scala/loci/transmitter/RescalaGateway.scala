package loci
package transmitter

import communicator.Connector
import messaging.ConnectionsBase

import _root_.rescala.core.{ReSerializable, Scheduler, Struct}
import _root_.rescala.interface.RescalaInterface
import _root_.rescala.macros.cutOutOfUserComputation

protected[transmitter] trait RescalaGateway {
  private final val joinedId = 0
  private final val leftId = 1
  private final val connectedId = 2

  protected class RescalaGateway[R, S <: Struct](gateway: Gateway[R])(implicit
      ev: Connection[R, _],
      val scheduler: Scheduler[S]) extends RemoteGateway {
    val interface = RescalaInterface.interfaceFor(scheduler)

    import interface.{Event, Evt}

    @cutOutOfUserComputation lazy val joined: Event[Remote[R]] =
      gateway.cache(joinedId) {
        val event = Evt[Remote[R]]
        gateway.remoteJoined foreach event.fire
        event
      }

    @cutOutOfUserComputation lazy val left: Event[Remote[R]] =
      gateway.cache(leftId) {
        val event = Evt[Remote[R]]
        gateway.remoteLeft foreach event.fire
        event
      }

    protected def update(update: => Unit) = {
      gateway.remoteJoined foreach { _ => update }
      gateway.remoteLeft foreach { _ => update }
    }
  }

  implicit class RescalaMultipleGateway[R, S <: Struct](gateway: Gateway[R])(implicit
      ev: Connection[R, Multiple],
      override val scheduler: Scheduler[S]) extends RescalaGateway[R, S](gateway) {
    import interface.{Signal, Var}

    @cutOutOfUserComputation lazy val connected: Signal[Seq[Remote[R]]] =
      gateway.cache(connectedId) {
        implicit val serializer = ReSerializable.noSerializer[Seq[Remote[R]]]

        val signal = Var(gateway.remotes)
        update { signal.set(gateway.remotes) }
        signal
      }

    def connect(connector: Connector[ConnectionsBase.Protocol]) =
      gateway.connectRemote(connector)
  }

  implicit class RescalaOptionalGateway[R, S <: Struct](gateway: Gateway[R])(implicit
      ev: Connection[R, Optional],
      override val scheduler: Scheduler[S]) extends RescalaGateway[R, S](gateway) {
    import interface.{Signal, Var}

    @cutOutOfUserComputation lazy val connected: Signal[Option[Remote[R]]] =
      gateway.cache(connectedId) {
        implicit val serializer = ReSerializable.noSerializer[Option[Remote[R]]]

        val signal = Var(gateway.remote)
        update { signal.set(gateway.remote) }
        signal
      }

    def connect(connector: Connector[ConnectionsBase.Protocol]) =
      gateway.connectRemote(connector)
  }

  implicit class RescalaSingleGateway[R, S <: Struct](gateway: Gateway[R])(implicit
      ev: Connection[R, Single],
      override val scheduler: Scheduler[S]) extends RescalaGateway[R, S](gateway) {

    @cutOutOfUserComputation lazy val connected: Remote[R] =
      gateway.remote
  }
}
