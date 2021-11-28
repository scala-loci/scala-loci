package loci
package concepts

import loci.communicator.Connector
import loci.communicator.DirectConnectionSimulation
import loci.communicator.NetworkListener
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

@multitier object PeerIdExchangeSingleModule {
  @peer type Node <: { type Tie <: Single[Node] }
}

@multitier object PeerIdExchangeOptionalModule {
  @peer type Node <: { type Tie <: Optional[Node] }
}

@multitier object PeerIdExchangeClientServerModule {
  @peer type Server <: { type Tie <: Multiple[Client] }
  @peer type Client <: { type Tie <: Single[Server] }
}

@multitier object PeerIdExchangeP2PModule {
  @peer type Node <: { type Tie <: Multiple[Node] }
}

@multitier object PeerIdExchangeDynamicConnectionModule {
  @peer type Node <: { type Tie <: Multiple[Node] }

  def connect(c: Connector[DirectConnectionSimulation.SimulationProtocol]): Local[Unit] on Node =  on[Node] local { implicit! =>
    remote[Node].connect(c)
  }
}

class PeerIdExchangeSpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "Peer id exchange"

  it should "exchange peer ids between single-tied peers" in {
    val listener = new NetworkListener()
    val a = multitier start new Instance[PeerIdExchangeSingleModule.Node](
      contexts.Immediate.global,
      listen[PeerIdExchangeSingleModule.Node](listener)
    )
    val b = multitier start new Instance[PeerIdExchangeSingleModule.Node](
      contexts.Immediate.global,
      connect[PeerIdExchangeSingleModule.Node](listener.createConnector())
    )

    val idA = a.instance.current.map(_.retrieveUniquePeerId()).get
    val idB = b.instance.current.map(_.retrieveUniquePeerId()).get
    val remotePeerIdsA = a.instance.current.map(_.retrieveRemotePeerIds()).get
    val remotePeerIdsB = b.instance.current.map(_.retrieveRemotePeerIds()).get

    remotePeerIdsA.keySet should contain theSameElementsAs Set(idB)
    remotePeerIdsB.keySet should contain theSameElementsAs Set(idA)
  }

  it should "exchange peer ids between optional-tied peers" in {
    val listener = new NetworkListener()
    val a = multitier start new Instance[PeerIdExchangeOptionalModule.Node](
      contexts.Immediate.global,
      listen[PeerIdExchangeOptionalModule.Node](listener)
    )
    val b = multitier start new Instance[PeerIdExchangeOptionalModule.Node](
      contexts.Immediate.global,
      connect[PeerIdExchangeOptionalModule.Node](listener.createConnector())
    )

    val idA = a.instance.current.map(_.retrieveUniquePeerId()).get
    val idB = b.instance.current.map(_.retrieveUniquePeerId()).get
    val remotePeerIdsA = a.instance.current.map(_.retrieveRemotePeerIds()).get
    val remotePeerIdsB = b.instance.current.map(_.retrieveRemotePeerIds()).get

    remotePeerIdsA.keySet should contain theSameElementsAs Set(idB)
    remotePeerIdsB.keySet should contain theSameElementsAs Set(idA)
  }

  it should "exchange peer ids between multiple clients and single server" in {
    val listener = new NetworkListener()
    val server = multitier start new Instance[PeerIdExchangeClientServerModule.Server](
      contexts.Immediate.global,
      listen[PeerIdExchangeClientServerModule.Client](listener) and
        listen[PeerIdExchangeClientServerModule.Client](listener)
    )
    val clientA = multitier start new Instance[PeerIdExchangeClientServerModule.Client](
      contexts.Immediate.global,
      connect[PeerIdExchangeClientServerModule.Server](listener.createConnector())
    )
    val clientB = multitier start new Instance[PeerIdExchangeClientServerModule.Client](
      contexts.Immediate.global,
      connect[PeerIdExchangeClientServerModule.Server](listener.createConnector())
    )

    val idServer = server.instance.current.map(_.retrieveUniquePeerId()).get
    val idClientA = clientA.instance.current.map(_.retrieveUniquePeerId()).get
    val idClientB = clientB.instance.current.map(_.retrieveUniquePeerId()).get
    val remotePeerIdsServer = server.instance.current.map(_.retrieveRemotePeerIds()).get
    val remotePeerIdsClientA = clientA.instance.current.map(_.retrieveRemotePeerIds()).get
    val remotePeerIdsClientB = clientB.instance.current.map(_.retrieveRemotePeerIds()).get

    remotePeerIdsServer.keySet should contain theSameElementsAs Set(idClientA, idClientB)
    remotePeerIdsClientA.keySet should contain theSameElementsAs Set(idServer)
    remotePeerIdsClientB.keySet should contain theSameElementsAs Set(idServer)
  }

  it should "exchange peer ids in a P2P setting" in {
    val listenerA = new NetworkListener()
    val listenerB = new NetworkListener()
    val listenerC = new NetworkListener()
    val b = multitier start new Instance[PeerIdExchangeP2PModule.Node](
      contexts.Immediate.global,
      listen[PeerIdExchangeP2PModule.Node](listenerB)
    )
    val c = multitier start new Instance[PeerIdExchangeP2PModule.Node](
      contexts.Immediate.global,
      listen[PeerIdExchangeP2PModule.Node](listenerC)
    )
    val a = multitier start new Instance[PeerIdExchangeP2PModule.Node](
      contexts.Immediate.global,
      listen[PeerIdExchangeP2PModule.Node](listenerA) and
        connect[PeerIdExchangeP2PModule.Node](listenerB.createConnector()) and
        connect[PeerIdExchangeP2PModule.Node](listenerC.createConnector())
    )
    val d = multitier start new Instance[PeerIdExchangeP2PModule.Node](
      contexts.Immediate.global,
      connect[PeerIdExchangeP2PModule.Node](listenerA.createConnector()) and
        connect[PeerIdExchangeP2PModule.Node](listenerB.createConnector()) and
        connect[PeerIdExchangeP2PModule.Node](listenerC.createConnector())
    )

    val idA = a.instance.current.map(_.retrieveUniquePeerId()).get
    val idB = b.instance.current.map(_.retrieveUniquePeerId()).get
    val idC = c.instance.current.map(_.retrieveUniquePeerId()).get
    val idD = d.instance.current.map(_.retrieveUniquePeerId()).get
    val remotePeerIdsA = a.instance.current.map(_.retrieveRemotePeerIds()).get
    val remotePeerIdsB = b.instance.current.map(_.retrieveRemotePeerIds()).get
    val remotePeerIdsC = c.instance.current.map(_.retrieveRemotePeerIds()).get
    val remotePeerIdsD = d.instance.current.map(_.retrieveRemotePeerIds()).get

    remotePeerIdsA.keySet should contain theSameElementsAs Set(idB, idC, idD)
    remotePeerIdsB.keySet should contain theSameElementsAs Set(idA, idD)
    remotePeerIdsC.keySet should contain theSameElementsAs Set(idA, idD)
    remotePeerIdsD.keySet should contain theSameElementsAs Set(idA, idB, idC)
  }

  it should "exchange peer ids on a dynamically setup connection" in {
    val listener = new NetworkListener()
    val a = multitier start new Instance[PeerIdExchangeDynamicConnectionModule.Node](
      contexts.Immediate.global,
      listen[PeerIdExchangeDynamicConnectionModule.Node](listener)
    )
    val b = multitier start new Instance[PeerIdExchangeDynamicConnectionModule.Node](
      contexts.Immediate.global
    )

    val idA = a.instance.current.map(_.retrieveUniquePeerId()).get
    val idB = b.instance.current.map(_.retrieveUniquePeerId()).get
    val remotePeerIdsA = a.instance.current.map(_.retrieveRemotePeerIds()).get
    val remotePeerIdsB = b.instance.current.map(_.retrieveRemotePeerIds()).get

    remotePeerIdsA.keySet should contain theSameElementsAs Set()
    remotePeerIdsB.keySet should contain theSameElementsAs Set()

    b.instance.current.foreach { _ retrieve PeerIdExchangeDynamicConnectionModule.connect(listener.createConnector()) }

    val newRemotePeerIdsA = a.instance.current.map(_.retrieveRemotePeerIds()).get
    val newRemotePeerIdsB = b.instance.current.map(_.retrieveRemotePeerIds()).get

    newRemotePeerIdsA.keySet should contain theSameElementsAs Set(idB)
    newRemotePeerIdsB.keySet should contain theSameElementsAs Set(idA)
  }

}
