package loci
package concepts

import loci.communicator.NetworkListener
import loci.valueref.ValueRefAccessors.IllegalLocalAccess
import loci.valueref.ValueRefAccessors.NotConnectedToPeerWithId
import loci.valueref.ValueRefAccessors.PeerValueCacheMiss
import loci.valueref._
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import transmitter.Serializables._

import java.util.UUID
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

@multitier object ValueRefModule {
  @peer type Node <: { type Tie <: Optional[Node] }

  def generateRef(x: Int): String at Node on Node = on[Node] { implicit! =>
    val s = s"value $x"
    val ref = remote ref s
    ref
  }

  def accessRef(ref: String at Node): Future[String] on Node = on[Node] { implicit! =>
    ref.deref
  }

  def accessRefLocally(ref: String at Node): String on Node = on[Node] { implicit! =>
    ref.derefLocally
  }
}

class ValueRefSpec extends AsyncFlatSpec with Matchers with NoLogging {
  behavior of "value references"

  override def executionContext: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

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
      val ref: String at ValueRefModule.Node =
        nodeA.instance.current.map { _.retrieve(ValueRefModule.generateRef(x)) }.get
      ref
    }

    Future.sequence((refs zip numbers).reverse.flatMap {
      case (ref, x) =>
        nodeB.instance.current.map {
          _.retrieve[Future[String]](ValueRefModule.accessRef(ref)).map { _ -> s"value $x" }
        }
    }).map(_.unzip).map {
      case (actual, expected) => actual shouldEqual expected
    }
  }

  it should "generate a value reference on a node and access them on the same node" in {
    val node = multitier start new Instance[ValueRefModule.Node](
      contexts.Immediate.global
    )

    val ref: String at ValueRefModule.Node = node.instance.current.map { _.retrieve(ValueRefModule.generateRef(42)) }.get
    node.instance.current.map { _.retrieve(ValueRefModule.accessRef(ref)).map { _ shouldEqual "value 42" } }.get
  }

  it should "fail when accessing a value reference on an instance that has no connection to the referenced peer" in {
    val nodeA = multitier start new Instance[ValueRefModule.Node](contexts.Immediate.global)
    val nodeB = multitier start new Instance[ValueRefModule.Node](contexts.Immediate.global)

    val ref: String at ValueRefModule.Node =
      nodeA.instance.current.map { _.retrieve(ValueRefModule.generateRef(42)) }.get

    nodeB.instance.current.map {
      _.retrieve[Future[String]](ValueRefModule.accessRef(ref)).failed.map {
        _ shouldBe a[NotConnectedToPeerWithId]
      }
    }.get
  }

  it should "fail when accessing a value reference that is not present in the cache of the peer the value should exist on" in {
    val listener = new NetworkListener
    val nodeA = multitier start new Instance[ValueRefModule.Node](
      contexts.Immediate.global,
      listen[ValueRefModule.Node](listener)
    )
    val nodeB = multitier start new Instance[ValueRefModule.Node](
      contexts.Immediate.global,
      connect[ValueRefModule.Node](listener.createConnector())
    )

    val ref: String at ValueRefModule.Node =
      nodeA.instance.current.map { _.retrieve(ValueRefModule.generateRef(42)) }.get
    val fakeRef = ref.copy[String, ValueRefModule.Node](valueId = UUID.randomUUID())

    nodeB.instance.current.map {
      _.retrieve[Future[String]](ValueRefModule.accessRef(fakeRef)).failed.map {
        _ shouldBe a[PeerValueCacheMiss]
      }
    }.get
  }

  it should "access a value reference locally when it lives on the accessing peer" in {
    val node = multitier start new Instance[ValueRefModule.Node](contexts.Immediate.global)

    val ref: String at ValueRefModule.Node =
      node.instance.current.map { _.retrieve(ValueRefModule.generateRef(42)) }.get

    node.instance.current.map { _.retrieve[String](ValueRefModule.accessRefLocally(ref)) }.get shouldEqual "value 42"
  }

  it should "fail accessing a value reference locally when it does not live on the accessing peer" in {
    val listener = new NetworkListener
    val nodeA = multitier start new Instance[ValueRefModule.Node](
      contexts.Immediate.global,
      listen[ValueRefModule.Node](listener)
    )
    val nodeB = multitier start new Instance[ValueRefModule.Node](
      contexts.Immediate.global,
      connect[ValueRefModule.Node](listener.createConnector())
    )

    val ref: String at ValueRefModule.Node =
      nodeA.instance.current.map { _.retrieve(ValueRefModule.generateRef(42)) }.get

    an[IllegalLocalAccess] shouldBe thrownBy {
      nodeB.instance.current.map { _.retrieve[String](ValueRefModule.accessRefLocally(ref)) }.get
    }
  }

}
