package loci
package concepts

import loci.communicator.NetworkListener
import loci.valueref._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import transmitter.Serializables._

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@multitier object ValueRefTransmissionModule {
  @peer type Node <: { type Tie <: Single[Node] }

  def generateValueRef(x: Int): Int via Node on Node = on[Node] { implicit! =>
    x.asValueRef
  }

  def transmitValueRef(ref: Int via Node): Future[Int via Node] on Node = on[Node] { implicit! =>
    on[Node].run.capture(ref)(implicit! => ref).asLocal
  }
}

class ValueRefTransmissionSpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "Transmitting value references"

  implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  it should "serialize and deserialize transmitted value references" in {
    val listener = new NetworkListener
    val nodeA = multitier start new Instance[ValueRefTransmissionModule.Node](
      contexts.Immediate.global,
      listen[ValueRefTransmissionModule.Node](listener)
    )
    val nodeB = multitier start new Instance[ValueRefTransmissionModule.Node](
      contexts.Immediate.global,
      connect[ValueRefTransmissionModule.Node](listener.createConnector())
    )

    val x = 42
    val ref: ValueRef[Int, ValueRefTransmissionModule.Node] =
      nodeA.instance.current.map { _.retrieve(ValueRefTransmissionModule.generateValueRef(x)) }.get

    nodeA.instance.current.foreach {
      a => a.retrieve(ValueRefTransmissionModule.transmitValueRef(ref)).map {
        returnedRef: ValueRef[Int, ValueRefTransmissionModule.Node] =>
          returnedRef shouldEqual ref
      }
    }
  }

}
