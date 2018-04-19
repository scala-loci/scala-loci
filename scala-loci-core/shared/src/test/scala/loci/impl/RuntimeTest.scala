package loci
package impl

import util._
import transmission._
import org.scalatest._
import scala.collection.mutable.ListBuffer

class RuntimeSpec extends FlatSpec with Matchers {
  val peerImplOps = new PeerImpl.Ops(null) {
    override def dispatch(request: String, id: AbstractionId, ref: AbstractionRef) = scala.util.Try { "" }
    override def main() = { }
    override def terminating() = { }
    override def error() = { }
    override def fatal() = { }
  }

  case object New
  case object ConstraintsViolated

  case class Receive(messageType: Option[Value], message: String)

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
        server.Connection,
        server,
        peerTypeOf[app.Server],
        { (ec, connections, remotes, futureRemotes) =>
          events += Server(New)

          connections.receive += { case (_, Message(_, properties, payload)) =>
            events += Server(Receive(properties("Type"), payload))
          }

          connections.constraintsViolated += { _ => events += Server(ConstraintsViolated) }

          connections.remoteJoined += { remote =>
            connections send (
              remote,
              ChannelMessage("dummyType", "dummyChannel", None, "hello from server"))
          }

          new System(ec, connections, remotes, futureRemotes, peerImplOps).main
        })

      Runtime.run(
        client.Connection,
        client,
        peerTypeOf[app.Client],
        { (ec, connections, remotes, futureRemotes) =>
          events += Client0(New)

          connections.receive += { case (_, Message(_, properties, payload)) =>
            events += Client0(Receive(properties("Type"), payload))
          }

          connections.constraintsViolated += { _ => events += Client0(ConstraintsViolated) }

          connections send (
            connections.remotes.head,
            ChannelMessage("dummyType", "dummyChannel", None, "hello from client0"))

          new System(ec, connections, remotes, futureRemotes, peerImplOps).main
        })

      Runtime.run(
        client.Connection,
        client,
        peerTypeOf[app.Client],
        { (ec, connections, remotes, futureRemotes) =>
          events += Client1(New)

          connections.receive += { case (_, Message(_, properties, payload)) =>
            events += Client1(Receive(properties("Type"), payload))
          }

          connections.constraintsViolated += { _ => events += Client1(ConstraintsViolated) }

          connections send (
            connections.remotes.head,
            ChannelMessage("dummyType", "dummyChannel", None, "hello from client1"))

          new System(ec, connections, remotes, futureRemotes, peerImplOps).main
        })

      listener.run

      serverRuntime.terminate


      events should have size 13


      val Seq(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12) = events


      Seq(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10) should contain inOrder (
        Server(New), Server(Receive(Some(Value("dummyType")), "hello from client0")))

      Seq(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10) should contain inOrder (
        Server(New), Server(Receive(Some(Value("dummyType")), "hello from client1")))

      Seq(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10) should contain inOrder (
        Server(New), Server(Receive(Some(Value("Started")), "")))

      Seq(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10) shouldNot contain inOrder (
        Server(Receive(Some(Value("Started")), "")), Server(New))

      exactly (2, events) should be (Server(Receive(Some(Value("Started")), "")))


      Seq(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10) should contain inOrder (
        Client0(New), Client0(Receive(Some(Value("dummyType")), "hello from server")))

      Seq(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10) should contain inOrder (
        Client0(New), Server(Receive(Some(Value("Started")), "")))


      Seq(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10) should contain inOrder (
        Client1(New), Client1(Receive(Some(Value("dummyType")), "hello from server")))

      Seq(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10) should contain inOrder (
        Client1(New), Server(Receive(Some(Value("Started")), "")))


      Seq(_11, _12) should contain allOf (
        Client0(ConstraintsViolated), Client1(ConstraintsViolated))
    }
  }
}
