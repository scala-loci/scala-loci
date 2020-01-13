package loci
package runtime

import org.scalatest._
import scala.util.Success

class SystemSpec extends FlatSpec with Matchers with NoLogging {
  behavior of "System"

  it should "handle remote access correctly" in {
    for (seed <- 0 to 5) {
      val listener = new NetworkListener(deferred = true)

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
        { (ties, context, connections, connected, connecting) =>
          val instance = new ServerClientApp.$loci$peer$Server {
            protected def $loci$sys$create = new System(this, None, false, ties, context, connections, connected, connecting)
          }
          serverSystem = instance.$loci$sys
          instance.$loci$sys.start()
          instance
        })

      Runtime.start(
        ServerClientApp.$loci$peer$sig$Client,
        ServerClientApp.$loci$peer$ties$Client,
        contexts.Immediate.global,
        (connect[ServerClientApp.Server] { listener.createConnector }).setup(
          ServerClientApp.$loci$peer$sig$Client,
          ServerClientApp.$loci$peer$ties$Client.keys.flatMap(_.bases).toList),
        { (ties, context, connections, connected, connecting) =>
          val instance = new { override val id = 20 } with ServerClientApp.$loci$peer$Client {
            protected def $loci$sys$create = new System(this, None, false, ties, context, connections, connected, connecting)
          }
          client0System = instance.$loci$sys
          instance.$loci$sys.start()
          instance
        })

      Runtime.start(
        ServerClientApp.$loci$peer$sig$Client,
        ServerClientApp.$loci$peer$ties$Client,
        contexts.Immediate.global,
        (connect[ServerClientApp.Server] { listener.createConnector }).setup(
          ServerClientApp.$loci$peer$sig$Client,
          ServerClientApp.$loci$peer$ties$Client.keys.flatMap(_.bases).toList),
        { (ties, context, connections, connected, connecting) =>
          val instance = new { override val id = 21 } with ServerClientApp.$loci$peer$Client {
            protected def $loci$sys$create = new System(this, None, false, ties, context, connections, connected, connecting)
          }
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
        requestResult = true)

      val Seq(res3) = serverSystem.invokeRemoteAccess(
        (),
        ServerClientApp.$loci$val$loci$runtime$ServerClientApp$0,
        ServerClientApp.$loci$peer$sig$Client,
        Seq(serverSystem.remoteReferences(
          ServerClientApp.$loci$peer$sig$Client,
          Seq.empty,
          earlyAccess = true)(1)),
        requestResult = true)

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
