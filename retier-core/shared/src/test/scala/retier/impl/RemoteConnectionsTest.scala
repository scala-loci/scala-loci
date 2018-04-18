package retier
package impl

import org.scalatest._
import scala.collection.mutable.ListBuffer

class RemoteConnectionsSpec extends FlatSpec with Matchers {
  val dummyType = (PeerType deserialize "Dummy{Peer}").get
  val serverType = (PeerType deserialize "MyServer{Server{Peer}}").get
  val superServerType = (PeerType deserialize "Server{Peer}").get
  val clientType = (PeerType deserialize "Client{Peer}").get
  val nodeType = (PeerType deserialize "Node{Peer}").get
  val peerType = (PeerType deserialize "Peer").get

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
    val dummy = new RemoteConnections(dummyType, Map(peerType -> MultipleConnection))
    val server = new RemoteConnections(serverType, Map(clientType -> MultipleConnection))
    val client0 = new RemoteConnections(clientType, Map(superServerType -> SingleConnection))
    val client1 = new RemoteConnections(clientType, Map(serverType -> SingleConnection, dummyType -> SingleConnection))
    val node0 = new RemoteConnections(nodeType, Map(nodeType -> SingleConnection))
    val node1 = new RemoteConnections(nodeType, Map(nodeType -> SingleConnection))

    val events = ListBuffer.empty[Any]

    Seq(server -> Server.apply _,
        client0 -> Client0.apply _, client1 -> Client1.apply _,
        node0 -> Node.apply _, node1 -> Node.apply _) foreach {
      case (peer, event) =>
        peer.remoteJoined += { _ => events += event(Joined) }
        peer.remoteLeft += { _ => events += event(Left) }
        peer.constraintsSatisfied += { _ => events += event(ConstraintsSatisfied) }
        peer.constraintsViolated += { _ => events += event(ConstraintsViolated) }
        peer.receive += { case (_, Message(_, _, payload)) => events += event(Receive(payload)) }
    }

    (events, dummy, server, client0, client1, node0, node1)
  }


  behavior of "RemoteConnections"

  it should "handle connections correctly for terminating client" in {
    val (events, _, server, client0, _, _, _) = setup
    val listener = new NetworkListener

    server.listen(listener, clientType)
    client0.request(listener.createRequestor, serverType)
    client0.terminate


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

    server.listen(listener, clientType)
    client0.request(listener.createRequestor, serverType)
    server.terminate

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
      val requestor = new NetworkRequestor(deferred = seed != 0, seed)

      node0.request(requestor.first, nodeType)
      node1.request(requestor.second, nodeType)

      requestor.run

      node0.terminate


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

      server.listen(listener, clientType)
      dummy.listen(dummyListener, clientType)
      client0.request(listener.createRequestor, superServerType)
      client1.request(listener.createRequestor, serverType)
      client1.request(listener.createRequestor, superServerType)
      client1.request(dummyListener.createRequestor, dummyType)

      listener.run
      dummyListener.run

      client0.send(client0.remotes(0), ChannelMessage("dummyChannel", "Test", None, "just a test"))

      server.run

      client1.send(client1.remotes(1), ChannelMessage("dummyChannel", "Test", None, "another test"))


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
