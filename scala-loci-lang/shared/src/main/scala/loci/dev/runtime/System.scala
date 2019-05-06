package loci.dev
package runtime

import loci.impl.RemoteConnections
import loci.transmitter.RemoteRef

import scala.concurrent.{ExecutionContext, Future}

class System(
    peer: PlacedValues,
    executionContext: ExecutionContext,
    remoteConnections: RemoteConnections,
    singleConnectedRemotes: Seq[RemoteRef],
    connectingRemotes: Seq[Future[RemoteRef]]) {

  def start(main: Option[() => Unit]): Unit = { }

  def invokeRemoteAccess[U, T](
      arguments: U,
      placedValue: PlacedValue[U, _, _, _, _, T],
      peer: Peer.Signature,
      remotes: Seq[RemoteRef],
      requestResult: Boolean): Seq[T] = ???

  def subjectiveValue[T, P](function: Remote[P] => T, remote: Remote[P]): T = ???
}
