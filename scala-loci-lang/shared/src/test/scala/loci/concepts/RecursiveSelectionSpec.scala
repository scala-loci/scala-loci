package loci
package concepts

import loci.communicator.NetworkListener
import loci.transmitter.RemoteAccessException
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import transmitter.Serializables._

import scala.concurrent.Future


@multitier object RecursiveSelectionModule {
  @peer type Node <: { type Tie <: Multiple[Node] }
  @peer type A <: Node { type Tie <: Multiple[Node] }
  @peer type B <: Node { type Tie <: Multiple[Node] }
  @peer type C <: Node { type Tie <: Multiple[Node] }

  var executeLocally: Local[Boolean] on Node = on[Node] { implicit! => true }

  def select(connected: Seq[Remote[Node]], self: SelfReference[Node]): Local[Remote[Node]] on Node = on[Node] { implicit! =>
    throw new NotImplementedError
  } and on[A] { implicit! =>
    if (executeLocally) { self } else { connected.flatMap(_.asRemote[B]).head }
  } and on[B] { implicit! =>
    if (executeLocally) { self } else { connected.flatMap(_.asRemote[C]).head }
  } and on[C] { implicit! =>
    if (executeLocally) { self } else { throw new RuntimeException }
  }

  def f(x: Int): String on Node = on[Node] { implicit! =>
    throw new NotImplementedError
  } and on[A] { implicit! =>
    s"A: $x"
  } and on[B] { implicit! =>
    s"B: $x"
  } and on[C] { implicit! =>
    s"C: $x"
  }

  def run(x: Int): Future[String] on Node = on[Node] { implicit! =>
    remoteAny.recursive[Node](select _).call(f(x)).asLocal
  }
}

@multitier object RecursiveSelectionWithLocalValuesModule {
  @peer type Node <: { type Tie <: Multiple[Node] }
  @peer type A <: Node { type Tie <: Multiple[Node] }
  @peer type B <: Node { type Tie <: Multiple[Node] }
  @peer type C <: Node { type Tie <: Multiple[Node] }

  def select(
    connected: Seq[Remote[Node]],
    self: SelfReference[Node],
    peerId: String
  ): Local[Remote[Node]] on Node = on[Node] { implicit! =>
    throw new NotImplementedError
  } and on[A] { implicit! =>
    if (peerId == "A") { self } else { connected.flatMap(_.asRemote[B]).head }
  } and on[B] { implicit! =>
    if (peerId == "B") { self } else { connected.flatMap(_.asRemote[C]).head }
  } and on[C] { implicit! =>
    if (peerId == "C") { self } else { throw new RuntimeException }
  }

  def f(): String on Node = on[Node] { implicit! =>
    throw new NotImplementedError
  } and on[A] { implicit! =>
    s"A"
  } and on[B] { implicit! =>
    s"B"
  } and on[C] { implicit! =>
    s"C"
  }

  def run(id: String): Future[String] on Node = on[Node] { implicit! =>
    remoteAny.recursive[Node](select(_, _, id)).call(f()).asLocal
  }
}

class RecursiveSelectionSpec extends AsyncFlatSpec with Matchers with NoLogging {
  behavior of "Recursive selection of executing peer of a remote call"

  it should "execute the call locally when SelfReference is selected and otherwise pass execution to the next peer" in {
    val listenerAB = new NetworkListener
    val listenerBC = new NetworkListener

    val nodeA = multitier start new Instance[RecursiveSelectionModule.A](
      contexts.Immediate.global,
      listen[RecursiveSelectionModule.B](listenerAB)
    )
    val nodeB = multitier start new Instance[RecursiveSelectionModule.B](
      contexts.Immediate.global,
      connect[RecursiveSelectionModule.A](listenerAB.createConnector()) and
        listen[RecursiveSelectionModule.C](listenerBC)
    )
    val nodeC = multitier start new Instance[RecursiveSelectionModule.C](
      contexts.Immediate.global,
      connect[RecursiveSelectionModule.B](listenerBC.createConnector())
    )

    nodeA.instance.current map { _ retrieve RecursiveSelectionModule.executeLocally shouldBe true }
    nodeB.instance.current map { _ retrieve RecursiveSelectionModule.executeLocally shouldBe true }
    nodeC.instance.current map { _ retrieve RecursiveSelectionModule.executeLocally shouldBe true }

    val a1 = nodeA.instance.current.map { _ retrieve[Future[String]] RecursiveSelectionModule.run(1) map { _ -> "A: 1" }}.get
    val b2 = nodeB.instance.current.map { _ retrieve[Future[String]] RecursiveSelectionModule.run(2) map { _ -> "B: 2" }}.get
    val c3 = nodeC.instance.current.map { _ retrieve[Future[String]] RecursiveSelectionModule.run(3) map { _ -> "C: 3" }}.get

    nodeA.instance.current foreach { _ retrieve (RecursiveSelectionModule.executeLocally = false) }

    nodeA.instance.current map { _ retrieve RecursiveSelectionModule.executeLocally shouldBe false }
    nodeB.instance.current map { _ retrieve RecursiveSelectionModule.executeLocally shouldBe true }
    nodeC.instance.current map { _ retrieve RecursiveSelectionModule.executeLocally shouldBe true }

    val b1 = nodeA.instance.current.map { _ retrieve[Future[String]] RecursiveSelectionModule.run(1) map { _ -> "B: 1" }}.get

    nodeB.instance.current foreach { _ retrieve (RecursiveSelectionModule.executeLocally = false) }

    nodeA.instance.current map { _ retrieve RecursiveSelectionModule.executeLocally shouldBe false }
    nodeB.instance.current map { _ retrieve RecursiveSelectionModule.executeLocally shouldBe false }
    nodeC.instance.current map { _ retrieve RecursiveSelectionModule.executeLocally shouldBe true }

    val c1 = nodeA.instance.current.map { _ retrieve[Future[String]] RecursiveSelectionModule.run(1) map { _ -> "C: 1" }}.get

    Future.sequence(Seq(a1, b2, c3, b1, c1)).map(_.unzip).map {
      case (actual, expected) => actual shouldEqual expected
    }

  }

  it should "throw an exception when all peers don't execute locally" in {
    val listenerAB = new NetworkListener
    val listenerBC = new NetworkListener

    val nodeA = multitier start new Instance[RecursiveSelectionModule.A](
      contexts.Immediate.global,
      listen[RecursiveSelectionModule.B](listenerAB)
    )
    val nodeB = multitier start new Instance[RecursiveSelectionModule.B](
      contexts.Immediate.global,
      connect[RecursiveSelectionModule.A](listenerAB.createConnector()) and
        listen[RecursiveSelectionModule.C](listenerBC)
    )
    val nodeC = multitier start new Instance[RecursiveSelectionModule.C](
      contexts.Immediate.global,
      connect[RecursiveSelectionModule.B](listenerBC.createConnector())
    )

    nodeA.instance.current foreach { _ retrieve (RecursiveSelectionModule.executeLocally = false) }
    nodeB.instance.current foreach { _ retrieve (RecursiveSelectionModule.executeLocally = false) }
    nodeC.instance.current foreach { _ retrieve (RecursiveSelectionModule.executeLocally = false) }

    nodeA.instance.current map { _ retrieve RecursiveSelectionModule.executeLocally shouldBe false }
    nodeB.instance.current map { _ retrieve RecursiveSelectionModule.executeLocally shouldBe false }
    nodeC.instance.current map { _ retrieve RecursiveSelectionModule.executeLocally shouldBe false }

    nodeA.instance.current.map {
      _.retrieve[Future[String]](RecursiveSelectionModule.run(1)).failed.map { _ shouldBe a[RemoteAccessException] }
    }.get
  }

  it should "use the value passed to the selection rule in its recursive executions correctly" in {
    val listenerAB = new NetworkListener
    val listenerBC = new NetworkListener

    val nodeA = multitier start new Instance[RecursiveSelectionWithLocalValuesModule.A](
      contexts.Immediate.global,
      listen[RecursiveSelectionWithLocalValuesModule.B](listenerAB)
    )
    val nodeB = multitier start new Instance[RecursiveSelectionWithLocalValuesModule.B](
      contexts.Immediate.global,
      connect[RecursiveSelectionWithLocalValuesModule.A](listenerAB.createConnector()) and
        listen[RecursiveSelectionWithLocalValuesModule.C](listenerBC)
    )
    val nodeC = multitier start new Instance[RecursiveSelectionWithLocalValuesModule.C](
      contexts.Immediate.global,
      connect[RecursiveSelectionWithLocalValuesModule.B](listenerBC.createConnector())
    )

    val aa = nodeA.instance.current.map { _ retrieve[Future[String]] RecursiveSelectionWithLocalValuesModule.run("A") map { _ -> "A" }}.get
    val bb = nodeB.instance.current.map { _ retrieve[Future[String]] RecursiveSelectionWithLocalValuesModule.run("B") map { _ -> "B" }}.get
    val cc = nodeC.instance.current.map { _ retrieve[Future[String]] RecursiveSelectionWithLocalValuesModule.run("C") map { _ -> "C" }}.get

    val ab = nodeA.instance.current.map { _ retrieve[Future[String]] RecursiveSelectionWithLocalValuesModule.run("B") map { _ -> "B" }}.get
    val ac = nodeA.instance.current.map { _ retrieve[Future[String]] RecursiveSelectionWithLocalValuesModule.run("C") map { _ -> "C" }}.get

    Future.sequence(Seq(aa, bb, cc, ab, ac)).map(_.unzip).map {
      case (actual, expected) => actual shouldEqual expected
    }
  }

}
