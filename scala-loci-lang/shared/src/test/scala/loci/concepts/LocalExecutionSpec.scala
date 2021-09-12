package loci
package concepts

import loci.communicator.NetworkListener
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import transmitter.Serializables._


@multitier object Module {
  @peer type Node <: {type Tie <: Multiple[Node]}

  def select(connected: Seq[Remote[Node]], self: SelfReference[Node]): Local[Remote[Node]] on Node = on[Node] { implicit! =>
    if (executeLocally) self else connected.head
  }

  var value: Int on Node = on[Node] { implicit ! => 0 }
  var executeLocally: Boolean on Node = on[Node] { implicit! => true }

  def incValue(): Unit on Node = on[Node] { implicit ! =>
    onAny.apply[Node](select _).run { implicit ! =>
      value += 1
    }
  }
}

class LocalExecutionSpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "Local execution of remote blocks"

  it should "execute blocks locally only when SelfReference is selected" in {
    val listener = new NetworkListener

    val nodeA = multitier start new Instance[Module.Node](
      contexts.Immediate.global,
      listen[Module.Node](listener)
    )
    val nodeB = multitier start new Instance[Module.Node](
      contexts.Immediate.global,
      connect[Module.Node](listener.createConnector())
    )

    nodeA.instance.current map { _ retrieve Module.executeLocally shouldEqual true }
    nodeB.instance.current map { _ retrieve Module.executeLocally shouldEqual true }

    nodeA.instance.current map { _ retrieve Module.value shouldEqual 0 }
    nodeB.instance.current map { _ retrieve Module.value shouldEqual 0 }

    nodeA.instance.current foreach { _ retrieve Module.incValue() }
    nodeA.instance.current map { _ retrieve Module.value shouldEqual 1 }
    nodeB.instance.current map { _ retrieve Module.value shouldEqual 0 }

    nodeA.instance.current foreach { _ retrieve (Module.executeLocally = false) }
    nodeA.instance.current map { _ retrieve Module.executeLocally shouldEqual false }
    nodeB.instance.current map { _ retrieve Module.executeLocally shouldEqual true }

    nodeA.instance.current foreach { _ retrieve Module.incValue() }
    nodeA.instance.current map { _ retrieve Module.value shouldEqual 1 }
    nodeB.instance.current map { _ retrieve Module.value shouldEqual 1 }

    nodeB.instance.current foreach { _ retrieve Module.incValue() }
    nodeA.instance.current map { _ retrieve Module.value shouldEqual 1 }
    nodeB.instance.current map { _ retrieve Module.value shouldEqual 2 }
  }

}
