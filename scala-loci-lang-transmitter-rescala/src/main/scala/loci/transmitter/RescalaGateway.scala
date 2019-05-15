package loci
package transmitter

import _root_.rescala.core.{Scheduler, Struct}
import _root_.rescala.macros.cutOutInReactiveMacro
import loci.communicator.Connector
import loci.messaging.ConnectionsBase

protected[transmitter] trait RescalaGateway {
  private final val joinedId = 0
  private final val leftId = 1
  private final val connectedId = 2

  protected class RescalaGateway[R, S <: Struct](gateway: Gateway[R])(implicit
      ev: Connection[R, _],
      val scheduler: Scheduler[S])
    extends RemoteGateway {
    import scheduler.{Event, Evt}

    lazy val joined: Event[Remote[R]] @cutOutInReactiveMacro =
      gateway.cache(joinedId) {
        val event = Evt[Remote[R]]
        gateway.remoteJoined notify event.fire
        event
      }

    lazy val left: Event[Remote[R]] @cutOutInReactiveMacro =
      gateway.cache(leftId) {
        val event = Evt[Remote[R]]
        gateway.remoteLeft notify event.fire
        event
      }

    protected def update(update: => Unit) = {
      gateway.remoteJoined notify { _ => update }
      gateway.remoteLeft notify { _ => update }
    }
  }

  implicit class RescalaMultipleGateway[R, S <: Struct](gateway: Gateway[R])(implicit
      ev: Connection[R, Multiple],
      override val scheduler: Scheduler[S])
    extends RescalaGateway[R, S](gateway) {
    import scheduler.{Signal, Var}

    lazy val connected: Signal[Seq[Remote[R]]] @cutOutInReactiveMacro =
      gateway.cache(connectedId) {
        val signal = Var(gateway.remotes)
        update { signal.set(gateway.remotes) }
        signal
      }

    def connect(connector: Connector[ConnectionsBase.Protocol]) =
      gateway.connectRemote(connector)
  }

  implicit class RescalaOptionalGateway[R, S <: Struct](gateway: Gateway[R])(implicit
      ev: Connection[R, Optional],
      override val scheduler: Scheduler[S])
    extends RescalaGateway[R, S](gateway) {
    import scheduler.{Signal, Var}

    lazy val connected: Signal[Option[Remote[R]]] @cutOutInReactiveMacro =
      gateway.cache(connectedId) {
        val signal = Var(gateway.remote)
        update { signal.set(gateway.remote) }
        signal
      }

    def connect(connector: Connector[ConnectionsBase.Protocol]) =
      gateway.connectRemote(connector)
  }

  implicit class RescalaSingleGateway[R, S <: Struct](gateway: Gateway[R])(implicit
      ev: Connection[R, Single],
      override val scheduler: Scheduler[S])
    extends RescalaGateway[R, S](gateway) {

    lazy val connected: Remote[R] @cutOutInReactiveMacro = gateway.remote
  }
}
