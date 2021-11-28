package loci
package runtime

import communicator.NetworkListener
import messaging.Message
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.UUID
import scala.collection.mutable
import scala.collection.Seq

@compatibility.nowarn("msg=multiarg infix syntax")
class RuntimeSpec extends AnyFlatSpec with Matchers with NoLogging {
  case object New
  case object ConstraintsViolated

  case class Receive(messageType: Seq[String], message: String)

  case class Server(event: Any)
  case class Client0(event: Any)
  case class Client1(event: Any)


  behavior of "Runtime"

  it should "handle setup, messaging and termination correctly" in {
    for (seed <- 0 to 5) {
      val events = mutable.ListBuffer.empty[Any]

      val listener = new NetworkListener(deferred = seed != 0, seed)

      val serverId = UUID.randomUUID()
      val client0Id = UUID.randomUUID()
      val client1Id = UUID.randomUUID()

      val serverRuntime = Runtime.start(
        ServerClientApp.$loci$peer$sig$Server,
        serverId,
        ServerClientApp.$loci$peer$ties$Server,
        contexts.Immediate.global,
        (listen[ServerClientApp.Client] { listener }).setup(
          ServerClientApp.$loci$peer$sig$Server,
          ServerClientApp.$loci$peer$ties$Server.keys.flatMap(_.bases).toList),
        { (sig, peerId, ties, context, connections, connected, connecting) =>
          events += Server(New)

          connections.receive foreach { remoteMessage =>
            val (_, Message(_, properties, payload)) = remoteMessage
            events += Server(Receive(properties("Type"), payload.decodeString))
          }

          connections.constraintsViolated foreach { _ => events += Server(ConstraintsViolated) }

          connections.remoteJoined foreach { remote =>
            connections.send(
              remote,
              ChannelMessage(ChannelMessage.Type.Update, "dummyChannel", None, MessageBuffer encodeString "hello from server"))
          }

          val instance = new ServerClientApp.$loci$peer$loci$runtime$ServerClientApp$Client {
            protected def $loci$sys$create = new System(this, sig, peerId, None, false, ties, context, connections, connected, connecting)
          }
          instance.$loci$sys.start()
          instance
        })


      val client0Runtime = Runtime.start(
        ServerClientApp.$loci$peer$sig$Client,
        client0Id,
        ServerClientApp.$loci$peer$ties$Client,
        contexts.Immediate.global,
        (connect[ServerClientApp.Server] { listener.createConnector() }).setup(
          ServerClientApp.$loci$peer$sig$Client,
          ServerClientApp.$loci$peer$ties$Client.keys.flatMap(_.bases).toList),
        { (sig, peerId, ties, context, connections, connected, connecting) =>
          events += Client0(New)

          connections.receive foreach { remoteMessage =>
            val (_, Message(_, properties, payload)) = remoteMessage
            events += Client0(Receive(properties("Type"), payload.decodeString))
          }

          connections.constraintsViolated foreach { _ => events += Client0(ConstraintsViolated) }

          connections.send(
            connections.remotes.head,
            ChannelMessage(ChannelMessage.Type.Update, "dummyChannel", None, MessageBuffer encodeString "hello from client0"))

          val instance = new ServerClientApp.$loci$peer$loci$runtime$ServerClientApp$Server {
            protected def $loci$sys$create = new System(this, sig, peerId, None, false, ties, context, connections, connected, connecting)
          }
          instance.$loci$sys.start()
          instance
        })

      val client1Runtime = Runtime.start(
        ServerClientApp.$loci$peer$sig$Client,
        client1Id,
        ServerClientApp.$loci$peer$ties$Client,
        contexts.Immediate.global,
        (connect[ServerClientApp.Server] { listener.createConnector() }).setup(
          ServerClientApp.$loci$peer$sig$Client,
          ServerClientApp.$loci$peer$ties$Client.keys.flatMap(_.bases).toList),
        { (sig, peerId, ties, context, connections, connected, connecting) =>
          events += Client1(New)

          connections.receive foreach { remoteMessage =>
            val (_, Message(_, properties, payload)) = remoteMessage
            events += Client1(Receive(properties("Type"), payload.decodeString))
          }

          connections.constraintsViolated foreach { _ => events += Client1(ConstraintsViolated) }

          connections.send(
            connections.remotes.head,
            ChannelMessage(ChannelMessage.Type.Update, "dummyChannel", None, MessageBuffer encodeString "hello from client1"))

          val instance = new ServerClientApp.$loci$peer$loci$runtime$ServerClientApp$Server {
            protected def $loci$sys$create = new System(this, sig, peerId, None, false, ties, context, connections, connected, connecting)
          }
          instance.$loci$sys.start()
          instance
        })

      listener.run()

      serverRuntime.terminate()
      client0Runtime.terminate()
      client1Runtime.terminate()

      events should have size 17

      val Seq(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16) = events: @unchecked

      Seq(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14) should contain inOrder (
        Server(New), Server(Receive(Seq(ChannelMessage.Type.Update.toString), "hello from client0")))

      Seq(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14) should contain inOrder (
        Server(New), Server(Receive(Seq(ChannelMessage.Type.Update.toString), "hello from client1")))

      Seq(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14) should contain inOrder (
        Server(New), Server(Receive(Seq("Started"), "")))

      Seq(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14) shouldNot contain inOrder (
        Server(Receive(Seq("Started"), "")), Server(New))

      exactly (2, events) should be (Server(Receive(Seq("Started"), "")))

      Seq(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14) should contain inOrder (
        Client0(New), Client0(Receive(Seq(ChannelMessage.Type.Update.toString), "hello from server")))

      Seq(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14) should contain inOrder (
        Client0(New), Server(Receive(Seq("Started"), "")))

      Seq(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14) should contain inOrder (
        Client1(New), Client1(Receive(Seq(ChannelMessage.Type.Update.toString), "hello from server")))

      Seq(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14) should contain inOrder (
        Client1(New), Server(Receive(Seq("Started"), "")))

      Seq(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14) should contain inOrder (
        Server(Receive(Seq("PeerIdExchangeInit"), client0Id.toString)),
        Client0(Receive(Seq("PeerIdExchangeResponse"), serverId.toString))
      )

      Seq(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14) should contain inOrder (
        Server(Receive(Seq("PeerIdExchangeInit"), client1Id.toString)),
        Client1(Receive(Seq("PeerIdExchangeResponse"), serverId.toString))
      )

      Seq(_15, _16) should contain allOf (
        Client0(ConstraintsViolated), Client1(ConstraintsViolated))
    }
  }
}
