package loci
package embedding

import communicator._
import messaging._
import runtime.Peer

sealed trait Connections { self =>
  def setup(peer: Peer.Signature, peers: List[Peer.Signature]):
    Map[Peer.Signature, (
      List[Listener[ConnectionsBase.Protocol]],
      List[Connector[ConnectionsBase.Protocol]])]

  def and(other: Connections): Connections =
    new Connections {
      def setup(peer: Peer.Signature, peers: List[Peer.Signature]) = {
        val selfSetup = self.setup(peer, peers)
        val otherSetup = other.setup(peer, peers)

        ((selfSetup.keySet ++ otherSetup.keySet) map { key =>
          val (selfListeners, selfConnectors) =
            selfSetup.getOrElse(key, (List.empty, List.empty))
          val (setupListeners, setupConnectors) =
            otherSetup.getOrElse(key, (List.empty, List.empty))
          key -> (
            (selfListeners ++ setupListeners) ->
            (selfConnectors ++ setupConnectors))
        }).toMap
      }
    }
}

object Connections {
  def empty = new Connections {
    def setup(setupPeer: Peer.Signature, setupPeers: List[Peer.Signature]) =
      Map.empty
  }

  def connect(
      peer: Peer.Signature, connector: Connector[ConnectionsBase.Protocol]) = new Connections {
    def setup(setupPeer: Peer.Signature, setupPeers: List[Peer.Signature]) =
      Map(peer -> (List.empty -> List(connector)))
  }

  def connect(
      peer: Peer.Signature, factory: ConnectionSetupFactory[ConnectionsBase.Protocol],
      url: String, props: ConnectionSetupFactory.Properties) = new Connections {
    def setup(setupPeer: Peer.Signature, setupPeers: List[Peer.Signature]) =
      Map(peer -> (List.empty -> factory.connector(url, props).toList))
  }

  def listen(
      peer: Peer.Signature, listener: Listener[ConnectionsBase.Protocol]) = new Connections {
    def setup(setupPeer: Peer.Signature, setupPeers: List[Peer.Signature]) =
      Map(peer -> (List(listener) -> List.empty))
  }

  def listen(
      peer: Peer.Signature, factory: ConnectionSetupFactory[ConnectionsBase.Protocol],
      url: String, props: ConnectionSetupFactory.Properties) = new Connections {
    def setup(setupPeer: Peer.Signature, setupPeers: List[Peer.Signature]) =
      Map(peer -> (factory.listener(url, props).toList -> List.empty))
  }
}
