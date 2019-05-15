package loci.dev
package transmitter

import loci.communicator._

object RemoteGateway {
  trait Default { this: language.Gateway.type =>
    implicit class DefaultMultipleGateway[R](gateway: Gateway[R])(implicit
        ev: Connection[R, Multiple]) extends RemoteGateway {
      val joined = gateway.remoteJoined
      val left = gateway.remoteLeft
      def connected = gateway.remotes
      def connect(connector: Connector[ProtocolCommon]) =
        gateway.connectRemote(connector)
    }

    implicit class DefaultOptionalGateway[R](gateway: Gateway[R])(implicit
        ev: Connection[R, Optional]) extends RemoteGateway {
      val joined = gateway.remoteJoined
      val left = gateway.remoteLeft
      def connected = gateway.remote
      def connect(connector: Connector[ProtocolCommon]) =
        gateway.connectRemote(connector)
    }

    implicit class DefaultSingleGateway[R](gateway: Gateway[R])(implicit
        ev: Connection[R, Single]) extends RemoteGateway {
      val joined = gateway.remoteJoined
      val left = gateway.remoteLeft
      def connected = gateway.remote
      def connect(connector: Connector[ProtocolCommon]) =
        gateway.connectRemote(connector)
    }
  }

  sealed trait Access {
    implicit class MultipleGatewayAccess[R](gateway: Gateway[R])(implicit
        ev: Connection[R, _]) {

      val remoteJoined: loci.Notification[Remote[R]] = ev.remoteJoined
      val remoteLeft: loci.Notification[Remote[R]] = ev.remoteLeft
      def remotes: Seq[Remote[R]] = ev.multipleRemotes
      def connectRemote(connector: Connector[ProtocolCommon]): Unit =
        ev.connectRemote(connector)
    }
  }
}

trait RemoteGateway extends RemoteGateway.Access {
  implicit class OptionalGatewayAccess[R](gateway: Gateway[R])(implicit
      ev: Connection[R, Optional])
    extends MultipleGatewayAccess(gateway)(ev) {

    def remote: Option[Remote[R]] = ev.optionalRemote
  }

  implicit class SingleGatewayAccess[R](gateway: Gateway[R])(implicit
      ev: Connection[R, Single])
    extends MultipleGatewayAccess(gateway)(ev) {

    def remote: Remote[R] = ev.singleRemote
  }
}
