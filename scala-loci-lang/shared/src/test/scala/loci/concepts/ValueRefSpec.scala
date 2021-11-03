package loci
package concepts

import loci.communicator.NetworkListener
import loci.valueref._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import transmitter.Serializables._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

@multitier object ValueRefModule {
  @peer type Node <: { type Tie <: Single[Node] }

  def generateRef(x: Int): String via Node on Node = on[Node] { implicit! =>
    val s = s"value $x"
    val ref = s.asValueRef
    ref
  }

  def accessRef(ref: String via Node): Future[String] on Node = on[Node] { implicit! =>
    ref.getValue
  }
}

class ValueRefSpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "value references"

  it should "generate value references on one node and access them on the other" in {
    val listener = new NetworkListener
    val nodeA = multitier start new Instance[ValueRefModule.Node](
      contexts.Immediate.global,
      listen[ValueRefModule.Node](listener)
    )
    val nodeB = multitier start new Instance[ValueRefModule.Node](
      contexts.Immediate.global,
      connect[ValueRefModule.Node](listener.createConnector())
    )

    val numbers = 1 to 10

    val refs = numbers.map { x =>
      val ref: String via ValueRefModule.Node =
        nodeA.instance.current.map { _.retrieve(ValueRefModule.generateRef(x)) }.get
      ref
    }

    (refs zip numbers).reverse.foreach {
      case (ref, x) =>
        nodeB.instance.current.foreach {
          _.retrieve(ValueRefModule.accessRef(ref)).map {
            _ shouldEqual s"value $x"
          }
        }
    }
  }

}
