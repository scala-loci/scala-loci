package loci
package concepts

import loci.communicator.NetworkListener
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import transmitter.Serializables._

import scala.concurrent.ExecutionContext
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

class RecursiveSelectionSpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "Recursive selection of executing peer of a remote call"

  implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

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

    nodeA.instance.current map { _ retrieve RecursiveSelectionModule.run(1) map { _ shouldEqual "A: 1" }}
    nodeB.instance.current map { _ retrieve RecursiveSelectionModule.run(2) map { _ shouldEqual "B: 2" }}
    nodeC.instance.current map { _ retrieve RecursiveSelectionModule.run(3) map { _ shouldEqual "C: 3" }}

    nodeA.instance.current foreach { _ retrieve (RecursiveSelectionModule.executeLocally = false) }

    nodeA.instance.current map { _ retrieve RecursiveSelectionModule.executeLocally shouldBe false }
    nodeB.instance.current map { _ retrieve RecursiveSelectionModule.executeLocally shouldBe true }
    nodeC.instance.current map { _ retrieve RecursiveSelectionModule.executeLocally shouldBe true }

    nodeA.instance.current map { _ retrieve RecursiveSelectionModule.run(1) map { _ shouldEqual "B: 1" }}

    nodeB.instance.current foreach { _ retrieve (RecursiveSelectionModule.executeLocally = false) }

    nodeA.instance.current map { _ retrieve RecursiveSelectionModule.executeLocally shouldBe false }
    nodeB.instance.current map { _ retrieve RecursiveSelectionModule.executeLocally shouldBe false }
    nodeC.instance.current map { _ retrieve RecursiveSelectionModule.executeLocally shouldBe true }

    nodeA.instance.current map { _ retrieve RecursiveSelectionModule.run(1) map { _ shouldEqual "C: 1" }}

    nodeC.instance.current foreach { _ retrieve (RecursiveSelectionModule.executeLocally = false) }

    nodeA.instance.current map { _ retrieve RecursiveSelectionModule.executeLocally shouldBe false }
    nodeB.instance.current map { _ retrieve RecursiveSelectionModule.executeLocally shouldBe false }
    nodeC.instance.current map { _ retrieve RecursiveSelectionModule.executeLocally shouldBe false }

    nodeA.instance.current foreach { _ retrieve RecursiveSelectionModule.run(1) map { result => a[RuntimeException] shouldBe thrownBy(result) } }
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

    nodeA.instance.current map { _ retrieve RecursiveSelectionWithLocalValuesModule.run("A") map { _ shouldEqual "A" }}
    nodeB.instance.current map { _ retrieve RecursiveSelectionWithLocalValuesModule.run("B") map { _ shouldEqual "B" }}
    nodeC.instance.current map { _ retrieve RecursiveSelectionWithLocalValuesModule.run("C") map { _ shouldEqual "C" }}

    nodeA.instance.current map { _ retrieve RecursiveSelectionWithLocalValuesModule.run("B") map { _ shouldEqual "B" }}
    nodeA.instance.current map { _ retrieve RecursiveSelectionWithLocalValuesModule.run("C") map { _ shouldEqual "C" }}

    nodeB.instance.current foreach { _ retrieve RecursiveSelectionWithLocalValuesModule.run("A") map { result => a[RuntimeException] shouldBe thrownBy(result) } }
  }

}
