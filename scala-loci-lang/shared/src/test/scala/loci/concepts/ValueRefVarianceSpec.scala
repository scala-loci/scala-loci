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

  def generateRef(): Thing at Node on Node = on[Node] { implicit! =>
    remote ref ConcreteThing()
  }

  def accessRef(ref: Thing at Node): Future[Thing] on Node = on[Node] { implicit! =>
    ref.deref
  }
}

@multitier object ValueRefPeerTypeVarianceModule {
  @peer type Node <: { type Tie <: Optional[Node] }
  @peer type ConcreteNode <: Node { type Tie <: Optional[Node] }

  def generateRef(x: String): String at Node on ConcreteNode = on[ConcreteNode] { implicit! =>
    remote ref x
  }

  def accessRef(ref: String at Node): Future[String] on Node = on[Node] { implicit! =>
    ref.deref
  }
}

@multitier object ValueRefPeerCastingModule {
  @peergroup type Node <: { type Tie <: Optional[A] with Optional[B] }
  @peer type A <: Node { type Tie <: Optional[A] with Optional[B] }
  @peer type B <: Node { type Tie <: Optional[A] with Optional[B] }

  def generateRef(x: String): String at Node on Node = on[A] { implicit! =>
    remote ref s"$x at A"
  } and on[B] { implicit! =>
    remote ref s"$x at B"
  }

  def isViaA(ref: String at Node): Boolean on Node = { implicit! =>
    ref.at[A].isDefined
  }
}

@multitier object ValueRefDowncastModule {
  @peergroup type Node
  @peer type A <: Node
  @peer type B <: Node

  def generateRef(x: String): String at Node on Node = on[Node] { implicit! =>
    remote ref x
  }

  def isViaA(ref: String at Node): Boolean on Node = { implicit! =>
    ref.at[A].isDefined
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
      _.retrieve[Thing at ValueRefValueTypeVarianceModule.Node](ValueRefValueTypeVarianceModule.generateRef())
    }.get

    nodeB.instance.current.map {
      _.retrieve[Future[Thing]](ValueRefValueTypeVarianceModule.accessRef(ref)).map {
        _ shouldEqual ConcreteThing()
      }
    }.get
  }

  it should "generate value references with variant peer type and access them on another peer" in {
    val listener = new NetworkListener
    val nodeA = multitier start new Instance[ValueRefPeerTypeVarianceModule.ConcreteNode](
      contexts.Immediate.global,
      listen[ValueRefPeerTypeVarianceModule.ConcreteNode](listener)
    )
    val nodeB = multitier start new Instance[ValueRefPeerTypeVarianceModule.ConcreteNode](
      contexts.Immediate.global,
      connect[ValueRefPeerTypeVarianceModule.ConcreteNode](listener.createConnector())
    )

    val ref = nodeA.instance.current.map {
      _.retrieve[String at ValueRefPeerTypeVarianceModule.Node](ValueRefPeerTypeVarianceModule.generateRef("test"))
    }.get

    nodeB.instance.current.map {
      _.retrieve[Future[String]](ValueRefPeerTypeVarianceModule.accessRef(ref)).map {
        _ shouldEqual "test"
      }
    }.get
  }

  it should "cast a value reference to its actual type given the signature" in {
    val listener = new NetworkListener
    val nodeA = multitier start new Instance[ValueRefPeerCastingModule.A](
      contexts.Immediate.global,
      listen[ValueRefPeerCastingModule.B](listener)
    )
    val nodeB = multitier start new Instance[ValueRefPeerCastingModule.B](
      contexts.Immediate.global,
      connect[ValueRefPeerCastingModule.A](listener.createConnector())
    )

    val ref = nodeA.instance.current.map {
      _.retrieve[String at ValueRefPeerCastingModule.Node](ValueRefPeerCastingModule.generateRef("test"))
    }.get

    val isViaA = nodeB.instance.current.map {
      _.retrieve[Boolean](ValueRefPeerCastingModule.isViaA(ref))
    }.get
    isViaA shouldEqual true
  }

  it should "not cast a value reference to a type not matching the signature" in {
    val listener = new NetworkListener
    val b1 = multitier start new Instance[ValueRefPeerCastingModule.B](
      contexts.Immediate.global,
      listen[ValueRefPeerCastingModule.B](listener)
    )
    val b2 = multitier start new Instance[ValueRefPeerCastingModule.B](
      contexts.Immediate.global,
      connect[ValueRefPeerCastingModule.B](listener.createConnector())
    )

    val ref = b1.instance.current.map {
      _.retrieve[String at ValueRefPeerCastingModule.Node](ValueRefPeerCastingModule.generateRef("test"))
    }.get

    val isViaA = b2.instance.current.map {
      _.retrieve[Boolean](ValueRefPeerCastingModule.isViaA(ref))
    }.get
    isViaA shouldEqual false
  }

  it should "cast a value reference instantiated in peergroup context to its concrete instance peer" in {
    val a = multitier start new Instance[ValueRefDowncastModule.A](contexts.Immediate.global)

    val ref = a.instance.current.map {
      _.retrieve[String at ValueRefDowncastModule.Node](ValueRefDowncastModule.generateRef("test"))
    }.get

    val isViaA = a.instance.current.map {
      _.retrieve[Boolean](ValueRefDowncastModule.isViaA(ref))
    }.get
    isViaA shouldEqual true
  }

  it should "not cast a value reference instantiated in peergroup context to a non-matching concrete instance peer" in {
    val b = multitier start new Instance[ValueRefDowncastModule.B](contexts.Immediate.global)

    val ref = b.instance.current.map {
      _.retrieve[String at ValueRefDowncastModule.Node](ValueRefDowncastModule.generateRef("test"))
    }.get

    val isViaA = b.instance.current.map {
      _.retrieve[Boolean](ValueRefDowncastModule.isViaA(ref))
    }.get
    isViaA shouldEqual false
  }
}
