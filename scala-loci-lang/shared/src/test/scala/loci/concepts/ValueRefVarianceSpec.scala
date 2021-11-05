package loci
package concepts

import loci.communicator.NetworkListener
import loci.transmitter.transmittable.TransformingTransmittable
import loci.valueref._
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import transmitter.Serializables._

sealed trait Thing
case class ConcreteThing() extends Thing

object Thing {
  implicit val transmittable: TransformingTransmittable[Thing, String, Thing] = TransformingTransmittable(
    provide = (_, _) => "thing",
    receive = (_, _) => ConcreteThing()
  )
}

@multitier object ValueRefValueTypeVarianceModule {
  @peer type Node <: { type Tie <: Optional[Node] }

  def generateRef(): Thing via Node on Node = on[Node] { implicit! =>
    ConcreteThing().asValueRef
  }

  def accessRef(ref: Thing via Node): Future[Thing] on Node = on[Node] { implicit! =>
    ref.getValue
  }
}

class ValueRefVarianceSpec extends AsyncFlatSpec with Matchers with NoLogging {
  behavior of "variance for value references"

  override def executionContext: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

  it should "generate value references with variant value type and access them on another peer" in {
    val listener = new NetworkListener
    val nodeA = multitier start new Instance[ValueRefValueTypeVarianceModule.Node](
      contexts.Immediate.global,
      listen[ValueRefValueTypeVarianceModule.Node](listener)
    )
    val nodeB = multitier start new Instance[ValueRefValueTypeVarianceModule.Node](
      contexts.Immediate.global,
      connect[ValueRefValueTypeVarianceModule.Node](listener.createConnector())
    )

    val ref = nodeA.instance.current.map {
      _.retrieve[Thing via ValueRefValueTypeVarianceModule.Node](ValueRefValueTypeVarianceModule.generateRef())
    }.get

    nodeB.instance.current.map {
      _.retrieve[Future[Thing]](ValueRefValueTypeVarianceModule.accessRef(ref)).map {
        _ shouldEqual ConcreteThing()
      }
    }.get
  }
}
