package loci
package concepts

import loci.communicator.NetworkListener
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import transmitter.Serializables._

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@multitier object UnionPeerRemoteCallModule {
  @peer type A <: { type Tie <: Single[A | B] }
  @peer type B <: { type Tie <: Single[A] }

  def f(): Int on (A | B) = on[A] { implicit! => 1 } and on[B] { implicit! => 2 }
  def remoteF(): Future[Int] on A = on[A] { implicit! => remote[A | B].call(f()).asLocal }
}

class UnionPeerRemoteCallSpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "Remote call on union peer"

  implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  it should "execute the remote call on the correct peer" in {
    val listener = new NetworkListener

    val a = multitier start new Instance[UnionPeerRemoteCallModule.A](
      contexts.Immediate.global,
      listen[UnionPeerRemoteCallModule.B](listener)
    )
    val b = multitier start new Instance[UnionPeerRemoteCallModule.B](
      contexts.Immediate.global,
      connect[UnionPeerRemoteCallModule.A](listener.createConnector())
    )

    a.instance.current map { _ retrieve UnionPeerRemoteCallModule.f() shouldEqual 1 }
    b.instance.current map { _ retrieve UnionPeerRemoteCallModule.f() shouldEqual 2 }

    a.instance.current map { _ retrieve UnionPeerRemoteCallModule.remoteF() map { _ shouldEqual 2 }}
  }

}
