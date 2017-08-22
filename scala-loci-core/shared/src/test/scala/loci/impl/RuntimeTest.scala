package loci
package impl

import messaging.Message
import transmission._
import transmitter._
import org.scalatest._
import scala.collection.mutable.ListBuffer

class RuntimeSpec extends FlatSpec with Matchers {
  val peerImplOps = new PeerImpl.Ops(null) {
    override def dispatch(request: MessageBuffer, id: AbstractionId, ref: AbstractionRef) = scala.util.Try { MessageBuffer.empty }
    override def main() = { }
    override def terminating() = { }
    override def error() = { }
    override def fatal() = { }
  }

  case object New
  case object ConstraintsViolated

  case class Receive(messageType: Seq[String], message: String)

  case class Server(event: Any)
  case class Client0(event: Any)
  case class Client1(event: Any)


  behavior of "Runtime"

  it should "handle setup, messaging and termination correctly" in {
    for (seed <- 0 to 5) {
      val events = ListBuffer.empty[Any]

      val listener = new NetworkListener(deferred = seed != 0, seed)
      val app = new ServerClientApp(listener)

      val server = new app.Server
      val client = new app.Client

      val serverRuntime = Runtime.run(
        server.Tie,
        server,
        peerTypeOf[app.Server],
        { (ec, connections, remotes, futureRemotes) =>
          events += Server(New)

          connections.receive notify { remoteMessage =>
            val (_, Message(_, properties, payload)) = remoteMessage
            events += Server(Receive(properties("Type"), payload toString (0, payload.length)))
          }

          connections.constraintsViolated notify { _ => events += Server(ConstraintsViolated) }

          connections.remoteJoined notify { remote =>
            connections send (
              remote,
              ChannelMessage("dummyType", "dummyChannel", None, MessageBuffer fromString "hello from server"))
          }

          new System(ec, connections, remotes, futureRemotes, peerImplOps).main
        })

      Runtime.run(
        client.Tie,
        client,
        peerTypeOf[app.Client],
        { (ec, connections, remotes, futureRemotes) =>
          events += Client0(New)

          connections.receive notify { remoteMessage =>
            val (_, Message(_, properties, payload)) = remoteMessage
            events += Client0(Receive(properties("Type"), payload toString (0, payload.length)))
          }

          connections.constraintsViolated notify { _ => events += Client0(ConstraintsViolated) }

          connections send (
            connections.remotes.head,
            ChannelMessage("dummyType", "dummyChannel", None, MessageBuffer fromString "hello from client0"))

          new System(ec, connections, remotes, futureRemotes, peerImplOps).main
        })

      Runtime.run(
        client.Tie,
        client,
        peerTypeOf[app.Client],
        { (ec, connections, remotes, futureRemotes) =>
          events += Client1(New)

          connections.receive notify { remoteMessage =>
            val (_, Message(_, properties, payload)) = remoteMessage
            events += Client1(Receive(properties("Type"), payload toString (0, payload.length)))
          }

          connections.constraintsViolated notify { _ => events += Client1(ConstraintsViolated) }

          connections send (
            connections.remotes.head,
            ChannelMessage("dummyType", "dummyChannel", None, MessageBuffer fromString "hello from client1"))

          new System(ec, connections, remotes, futureRemotes, peerImplOps).main
        })

      listener.run

      serverRuntime.terminate


      events should have size 13


      val Seq(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12) = events


      Seq(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10) should contain inOrder (
        Server(New), Server(Receive(Seq("dummyType"), "hello from client0")))

      Seq(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10) should contain inOrder (
        Server(New), Server(Receive(Seq("dummyType"), "hello from client1")))

      Seq(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10) should contain inOrder (
        Server(New), Server(Receive(Seq("Started"), "")))

      Seq(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10) shouldNot contain inOrder (
        Server(Receive(Seq("Started"), "")), Server(New))

      exactly (2, events) should be (Server(Receive(Seq("Started"), "")))


      Seq(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10) should contain inOrder (
        Client0(New), Client0(Receive(Seq("dummyType"), "hello from server")))

      Seq(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10) should contain inOrder (
        Client0(New), Server(Receive(Seq("Started"), "")))


      Seq(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10) should contain inOrder (
        Client1(New), Client1(Receive(Seq("dummyType"), "hello from server")))

      Seq(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10) should contain inOrder (
        Client1(New), Server(Receive(Seq("Started"), "")))


      Seq(_11, _12) should contain allOf (
        Client0(ConstraintsViolated), Client1(ConstraintsViolated))
    }
  }
}
