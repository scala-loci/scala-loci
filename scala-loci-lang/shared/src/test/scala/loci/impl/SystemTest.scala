package loci
package impl

import org.scalatest._
import scala.util.Success

class SystemSpec extends FlatSpec with Matchers {
  behavior of "System"

  it should "handle remote access correctly" in {
    for (seed <- 0 to 5) {
      val listener = new NetworkListener(deferred = true)
      val app = new ServerClientApp(listener)

      val server = new app.Server
      val client = new app.Client

      val ServerImpl = app.Server.$$loci$peer
      val ClientImpl = app.Client.$$loci$peer

      var serverSystem: System = null
      var client0System: System = null
      var client1System: System = null

      Runtime.run(
        server.Tie,
        server,
        peerTypeOf[app.Server],
        { (ec, connections, remotes, futureRemotes) =>
          serverSystem = (new app.Server.$$loci$peer {
            override lazy val $$loci$system = new System(ec, connections, remotes, futureRemotes, this)
          }).$$loci$system.main

          serverSystem
        })

      Runtime.run(
        client.Tie,
        client,
        peerTypeOf[app.Client],
        { (ec, connections, remotes, futureRemotes) =>
          client0System = (new app.Client.$$loci$peer {
            override lazy val $$loci$system = new System(ec, connections, remotes, futureRemotes, this)
            override val id = 20
          }).$$loci$system.main

          client0System
        })

      Runtime.run(
        client.Tie,
        client,
        peerTypeOf[app.Client],
        { (ec, connections, remotes, futureRemotes) =>
          client1System = (new app.Client.$$loci$peer {
            override lazy val $$loci$system = new System(ec, connections, remotes, futureRemotes, this)
            override val id = 21
          }).$$loci$system.main

          client1System
        })

      val res0 = serverSystem.createMultipleTransmission[Int, app.Client, app.Server](ClientImpl.id).retrieveRemoteValues

      listener.run

      val res1 = serverSystem.createMultipleTransmission[Int, app.Client, app.Server](ClientImpl.id).retrieveRemoteValues

      val res2 = serverSystem.createSingleTransmission[Int, app.Client, app.Server](
        serverSystem.createPeerSelection[Int, app.Client](
          ClientImpl.id, serverSystem.remotes[app.Client] apply 0)).retrieveRemoteValue

      val res3 = serverSystem.createSingleTransmission[Int, app.Client, app.Server](
        serverSystem.createPeerSelection[Int, app.Client](
          ClientImpl.id, serverSystem.remotes[app.Client] apply 1)).retrieveRemoteValue

      val res4 = client0System.createSingleTransmission[Int, app.Server, app.Client](
        ServerImpl.square(4)).retrieveRemoteValue

      val res5 = client1System.createSingleTransmission[Int, app.Server, app.Client](
        ServerImpl.square(5)).retrieveRemoteValue


      res0 should be (empty)

      res1 map { _.value } should contain allOf (Some(Success(20)), Some(Success(21)))

      Seq(res2.value, res3.value) should contain allOf (Some(Success(20)), Some(Success(21)))

      res4.value should be (Some(Success(16)))

      res5.value should be (Some(Success(25)))
    }
  }
}
