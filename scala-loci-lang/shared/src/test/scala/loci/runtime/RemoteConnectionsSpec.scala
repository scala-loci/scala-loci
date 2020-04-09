package loci
package runtime

import messaging.Message

import org.scalatest._

import scala.collection.mutable

class RemoteConnectionsSpec extends FlatSpec with Matchers with NoLogging {
  val dummySig = Peer.Signature.deserialize("[Dummy,[],[[Peer,[],[]]],Module]").get
  val serverSig = Peer.Signature.deserialize("[MyServer,[],[[Server,[],[[Peer,[],[]]]]],Module]").get
  val superServerSig = Peer.Signature.deserialize("[Server,[],[[Peer,[],[]]],Module]").get
  val clientSig = Peer.Signature.deserialize("[Client,[],[[Peer,[],[]]],Module]").get
  val nodeSig = Peer.Signature.deserialize("[Node,[],[[Peer,[],[]]],Module]").get
  val peerSig = Peer.Signature.deserialize("[Peer,[],[],Module]").get

  case object Joined
  case object Left
  case object ConstraintsSatisfied
  case object ConstraintsViolated

  case class Receive(message: String)

  case class Server(event: Any)
  case class Client0(event: Any)
  case class Client1(event: Any)
  case class Node(event: Any)

  def setup = {
    val dummy = new RemoteConnections(dummySig, Map(peerSig -> Peer.Tie.Multiple))
    val server = new RemoteConnections(serverSig, Map(clientSig -> Peer.Tie.Multiple))
    val client0 = new RemoteConnections(clientSig, Map(superServerSig -> Peer.Tie.Single))
    val client1 = new RemoteConnections(clientSig, Map(serverSig -> Peer.Tie.Single, dummySig -> Peer.Tie.Single))
    val node0 = new RemoteConnections(nodeSig, Map(nodeSig -> Peer.Tie.Single))
    val node1 = new RemoteConnections(nodeSig, Map(nodeSig -> Peer.Tie.Single))

    val events = mutable.ListBuffer.empty[Any]

    Seq(server -> Server.apply _,
        client0 -> Client0.apply _, client1 -> Client1.apply _,
        node0 -> Node.apply _, node1 -> Node.apply _) foreach {
      case (peer, event) =>
        peer.remoteJoined foreach { _ => events += event(Joined) }
        peer.remoteLeft foreach { _ => events += event(Left) }
        peer.constraintsSatisfied foreach { _ => events += event(ConstraintsSatisfied) }
        peer.constraintsViolated foreach { _ => events += event(ConstraintsViolated) }
        peer.receive foreach { remoteMessage =>
          val (_, Message(_, _, payload)) = remoteMessage
          events += event(Receive(payload.decodeString))
        }
    }

    (events, dummy, server, client0, client1, node0, node1)
  }


  behavior of "RemoteConnections"

  it should "handle connections correctly for terminating client" in {
    val (events, _, server, client0, _, _, _) = setup
    val listener = new NetworkListener

    server.listen(listener, clientSig)
    client0.connect(listener.createConnector(), serverSig)
    client0.terminate()


    events should have size 5

    val Seq(_0, _1, _2, _3, _4) = events

    Seq(_0, _1, _2) should contain theSameElementsAs Seq(
      Client0(Joined), Client0(ConstraintsSatisfied), Server(Joined))

    Seq(_3, _4) should contain theSameElementsAs Seq(
      Client0(Left), Server(Left))
  }

  it should "handle connections correctly for terminating server" in {
    val (events, _, server, client0, _, _, _) = setup
    val listener = new NetworkListener

    server.listen(listener, clientSig)
    client0.connect(listener.createConnector(), serverSig)
    server.terminate()

    events should have size 6

    val Seq(_0, _1, _2, _3, _4, _5) = events

    Seq(_0, _1, _2) should contain theSameElementsAs Seq(
      Client0(Joined), Client0(ConstraintsSatisfied), Server(Joined))

    Seq(_3, _4, _5) should contain theSameElementsAs Seq(
      Client0(Left), Client0(ConstraintsViolated), Server(Left))
  }

  it should "handle connections correctly for terminating node" in {
    for (seed <- 0 to 5) {
      val (events, _, _, _, _, node0, node1) = setup
      val connector = new NetworkConnector(deferred = seed != 0, seed)

      node0.connect(connector.first, nodeSig)
      node1.connect(connector.second, nodeSig)

      connector.run()

      node0.terminate()


      events should have size 7

      val Seq(_0, _1, _2, _3, _4, _5, _6) = events

      Seq(_0, _1, _2, _3) should contain theSameElementsAs Seq(
        Node(Joined), Node(Joined),
        Node(ConstraintsSatisfied), Node(ConstraintsSatisfied))

      Seq(_4, _5, _6) should contain theSameElementsAs Seq(
        Node(Left), Node(Left), Node(ConstraintsViolated))
    }
  }

  it should "handle connections correctly in a more complex example" in {
    for (seed <- 0 to 5) {
      val (events, dummy, server, client0, client1, _, _) = setup
      val listener = new NetworkListener(deferred = seed != 0, seed)
      val dummyListener = new NetworkListener(deferred = seed != 0, seed)

      server.listen(listener, clientSig)
      dummy.listen(dummyListener, clientSig)
      client0.connect(listener.createConnector(), superServerSig)
      client1.connect(listener.createConnector(), serverSig)
      client1.connect(listener.createConnector(), superServerSig)
      client1.connect(dummyListener.createConnector(), dummySig)

      listener.run()
      dummyListener.run()

      client0.send(client0.remotes(0), ChannelMessage(ChannelMessage.Type.Update, "Test", None, MessageBuffer encodeString "just a test"))

      server.run()

      client1.send(client1.remotes(1), ChannelMessage(ChannelMessage.Type.Update, "Test", None, MessageBuffer encodeString "another test"))


      events should have size 11

      val Seq(_, _, _, _, _, _, _, _, _, _9, _10) = events

      exactly (1, events) should be (Client0(Joined))

      exactly (3, events) should be (Client1(Joined))

      exactly (3, events) should be (Server(Joined))

      events should contain inOrder (
        Client0(Joined), Client0(ConstraintsSatisfied))

      events should contain inOrder (
        Client1(Joined), Client1(ConstraintsSatisfied))

      Seq(_9, _10) should contain allOf (
        Server(Receive("just a test")), Server(Receive("another test")))
    }
  }
}
