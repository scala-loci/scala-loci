package loci
package transmitter

import communicator.Connector
import messaging.ConnectionsBase

import _root_.rescala.interface.RescalaInterface
import _root_.rescala.operator.cutOutOfUserComputation

protected[transmitter] trait RescalaGateway {
  val interface: RescalaInterface
  import interface._

  private final val joinedId = 0
  private final val leftId = 1
  private final val connectedId = 2

  protected class RescalaGateway[R](gateway: Gateway[R])(implicit
      ev: Connection[R, _]) extends RemoteGateway {

    @cutOutOfUserComputation lazy val joined: Event[Remote[R]] =
      gateway.cache(joinedId) {
        val event = Evt[Remote[R]]()
        gateway.remoteJoined foreach event.fire
        event
      }

    @cutOutOfUserComputation lazy val left: Event[Remote[R]] =
      gateway.cache(leftId) {
        val event = Evt[Remote[R]]()
        gateway.remoteLeft foreach event.fire
        event
      }

    protected def update(update: => Unit) = {
      gateway.remoteJoined foreach { _ => update }
      gateway.remoteLeft foreach { _ => update }
    }
  }

  implicit class RescalaMultipleGateway[R](gateway: Gateway[R])(implicit
      ev: Connection[R, Multiple]) extends RescalaGateway[R](gateway) {

    @cutOutOfUserComputation lazy val connected: Signal[Seq[Remote[R]]] =
      gateway.cache(connectedId) {
        val signal = Var(gateway.remotes)
        update { signal.set(gateway.remotes) }
        signal
      }

    def connect(connector: Connector[ConnectionsBase.Protocol]) =
      gateway.connectRemote(connector)
  }

  implicit class RescalaOptionalGateway[R](gateway: Gateway[R])(implicit
      ev: Connection[R, Optional]) extends RescalaGateway[R](gateway) {

    @cutOutOfUserComputation lazy val connected: Signal[Option[Remote[R]]] =
      gateway.cache(connectedId) {
        val signal = Var(gateway.remote)
        update { signal.set(gateway.remote) }
        signal
      }

    def connect(connector: Connector[ConnectionsBase.Protocol]) =
      gateway.connectRemote(connector)
  }

  implicit class RescalaSingleGateway[R](gateway: Gateway[R])(implicit
      ev: Connection[R, Single]) extends RescalaGateway[R](gateway) {

    @cutOutOfUserComputation lazy val connected: Remote[R] =
      gateway.remote
  }
}
