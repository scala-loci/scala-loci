package loci
package runtime

import communicator.NetworkListener
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.UUID
import scala.util.Success

class SystemSpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "System"

  it should "handle remote access correctly" in {
    for (seed <- 0 to 5) {
      val listener = new NetworkListener(deferred = true, seed)

      var serverSystem: System = null
      var client0System: System = null
      var client1System: System = null

      Runtime.start(
        ServerClientApp.$loci$peer$sig$Server,
        ServerClientApp.$loci$peer$ties$Server,
        contexts.Immediate.global,
        (listen[ServerClientApp.Client] { listener }).setup(
          ServerClientApp.$loci$peer$sig$Server,
          ServerClientApp.$loci$peer$ties$Server.keys.flatMap(_.bases).toList),
        { (sig, peerId, ties, context, connections, connected, connecting) =>
          val instance = new ServerClientApp.$loci$peer$loci$runtime$ServerClientApp$Server {
            protected def $loci$sys$create = new System(this, sig, peerId, None, false, ties, context, connections, connected, connecting)
          }
          serverSystem = instance.$loci$sys
          instance.$loci$sys.start()
          instance
        })

      Runtime.start(
        ServerClientApp.$loci$peer$sig$Client,
        ServerClientApp.$loci$peer$ties$Client,
        contexts.Immediate.global,
        (connect[ServerClientApp.Server] { listener.createConnector() }).setup(
          ServerClientApp.$loci$peer$sig$Client,
          ServerClientApp.$loci$peer$ties$Client.keys.flatMap(_.bases).toList),
        { (sig, peerId, ties, context, connections, connected, connecting) =>
          val instance = new { override val id = 20 } with ServerClientApp.$loci$peer$loci$runtime$ServerClientApp$Client {
            protected def $loci$sys$create = new System(this, sig, peerId, None, false, ties, context, connections, connected, connecting)
          }: @compatibility.nowarn("msg=early initializers")
          client0System = instance.$loci$sys
          instance.$loci$sys.start()
          instance
        })

      Runtime.start(
        ServerClientApp.$loci$peer$sig$Client,
        ServerClientApp.$loci$peer$ties$Client,
        contexts.Immediate.global,
        (connect[ServerClientApp.Server] { listener.createConnector() }).setup(
          ServerClientApp.$loci$peer$sig$Client,
          ServerClientApp.$loci$peer$ties$Client.keys.flatMap(_.bases).toList),
        { (sig, peerId, ties, context, connections, connected, connecting) =>
          val instance = new { override val id = 21 } with ServerClientApp.$loci$peer$loci$runtime$ServerClientApp$Client {
            protected def $loci$sys$create = new System(this, sig, peerId, None, false, ties, context, connections, connected, connecting)
          }: @compatibility.nowarn("msg=early initializers")
          client1System = instance.$loci$sys
          instance.$loci$sys.start()
          instance
        })

      val res0 = serverSystem.invokeRemoteAccess(
        (),
        ServerClientApp.$loci$val$loci$runtime$ServerClientApp$0,
        ServerClientApp.$loci$peer$sig$Client,
        Seq.empty,
        requestResult = true)

      listener.run()

      val res1 = serverSystem.invokeRemoteAccess(
        (),
        ServerClientApp.$loci$val$loci$runtime$ServerClientApp$0,
        ServerClientApp.$loci$peer$sig$Client,
        Seq.empty,
        requestResult = true)

      val Seq(res2) = serverSystem.invokeRemoteAccess(
        (),
        ServerClientApp.$loci$val$loci$runtime$ServerClientApp$0,
        ServerClientApp.$loci$peer$sig$Client,
        Seq(serverSystem.remoteReferences(
          ServerClientApp.$loci$peer$sig$Client,
          Seq.empty,
          earlyAccess = true)(0)),
        requestResult = true): @unchecked

      val Seq(res3) = serverSystem.invokeRemoteAccess(
        (),
        ServerClientApp.$loci$val$loci$runtime$ServerClientApp$0,
        ServerClientApp.$loci$peer$sig$Client,
        Seq(serverSystem.remoteReferences(
          ServerClientApp.$loci$peer$sig$Client,
          Seq.empty,
          earlyAccess = true)(1)),
        requestResult = true): @unchecked

      val res4 = client0System.invokeRemoteAccess(
        4,
        ServerClientApp.$loci$val$loci$runtime$ServerClientApp$1,
        ServerClientApp.$loci$peer$sig$Server,
        Seq.empty,
        requestResult = true).head

      val res5 = client1System.invokeRemoteAccess(
        5,
        ServerClientApp.$loci$val$loci$runtime$ServerClientApp$1,
        ServerClientApp.$loci$peer$sig$Server,
        Seq.empty,
        requestResult = true).head

      serverSystem.terminate()
      client0System.terminate()
      client1System.terminate()


      res0 should be (empty)

      res1 map { _.value } should contain theSameElementsAs Seq(
        Some(Success(20)), Some(Success(21)))

      Seq(res2.value, res3.value) should contain theSameElementsAs Seq(
        Some(Success(20)), Some(Success(21)))

      res4.value should be (Some(Success(16)))

      res5.value should be (Some(Success(25)))
    }
  }
}
