package loci
package concepts

import loci.communicator.NetworkListener
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import transmitter.Serializables._


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

  def run(x: Int): String on Node = on[Node] { implicit! =>
    remoteAny.recursive[Node](select _).call(f(x)).asLocal_!
  }
}

class RecursiveSelectionSpec extends AnyFlatSpec with Matchers with NoLogging {
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

    nodeA.instance.current map { _ retrieve RecursiveSelectionModule.run(1) shouldEqual "A: 1" }
    nodeB.instance.current map { _ retrieve RecursiveSelectionModule.run(2) shouldEqual "B: 2" }
    nodeC.instance.current map { _ retrieve RecursiveSelectionModule.run(3) shouldEqual "C: 3" }

    nodeA.instance.current foreach { _ retrieve (RecursiveSelectionModule.executeLocally = false) }

    nodeA.instance.current map { _ retrieve RecursiveSelectionModule.executeLocally shouldBe false }
    nodeB.instance.current map { _ retrieve RecursiveSelectionModule.executeLocally shouldBe true }
    nodeC.instance.current map { _ retrieve RecursiveSelectionModule.executeLocally shouldBe true }

    nodeA.instance.current map { _ retrieve RecursiveSelectionModule.run(1) shouldEqual "B: 1" }

    nodeB.instance.current foreach { _ retrieve (RecursiveSelectionModule.executeLocally = false) }

    nodeA.instance.current map { _ retrieve RecursiveSelectionModule.executeLocally shouldBe false }
    nodeB.instance.current map { _ retrieve RecursiveSelectionModule.executeLocally shouldBe false }
    nodeC.instance.current map { _ retrieve RecursiveSelectionModule.executeLocally shouldBe true }

    nodeA.instance.current map { _ retrieve RecursiveSelectionModule.run(1) shouldEqual "C: 1" }

    nodeC.instance.current foreach { _ retrieve (RecursiveSelectionModule.executeLocally = false) }

    nodeA.instance.current map { _ retrieve RecursiveSelectionModule.executeLocally shouldBe false }
    nodeB.instance.current map { _ retrieve RecursiveSelectionModule.executeLocally shouldBe false }
    nodeC.instance.current map { _ retrieve RecursiveSelectionModule.executeLocally shouldBe false }

    a[RuntimeException] shouldBe thrownBy {
      nodeA.instance.current foreach { _ retrieve RecursiveSelectionModule.run(1) }
    }
  }

}
