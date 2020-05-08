package loci
package language

import Testing._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.Future

class TypeLevelEncodingSpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "Type Level Encoding"

  implicit class StaticTypeAssertion[T](v: T) {
    def staticAssertType[U](implicit ev: T =:= U): Unit = locally(ev)
  }

  it should "typecheck remote access" in {
    """trait Module {
      @peer type Component
      @peer type Server <: Component { type Tie <: Multiple[Client] with Optional[MobileClient] }
      @peer type Client <: Component { type Tie <: Single[Server] with Single[SpecialServer] }
      @peer type MobileClient <: Client { type Tie <: Single[Server] with Single[SpecialServer] }
      @peer type SpecialServer <: Server { type Tie <: Multiple[Client] }


      val server: Remote[Server] = ???
      val client: Remote[Client] = ???


      val a0: Int on Server = placed { implicit ev => 42 }
      val a1 = on[Server] { implicit! => a0 to server }
      val a2 = on[Server] sbj { implicit! => v: Remote[Client] => a0 to server }

      a1.staticAssertType[Int on Server]
      a2.staticAssertType[Int per Client on Server]


      val b0: Local[Int] on Client = placed { implicit! => 42 }
      val b1 = on[Client] local { implicit! => b0 to server }
      val b2 = on[Client] sbj { implicit! => v: Remote[Server] => b0 to server }

      b1.staticAssertType[Local[Int] on Client]
      b2.staticAssertType[Int per Server on Client]


      val c0: Int on Client = placed { implicit! => b0 }
      val c1 = on[Client] { implicit! => b0 }
      val c2 = on[Client] sbj { implicit! => v: Remote[Server] => b0 }

      c1.staticAssertType[Int on Client]
      c2.staticAssertType[Int per Server on Client]


      val d0: Int per Server on Client = placed { implicit! => v: Remote[Server] => 42 }
      val d1 = on[Client] { implicit! => c0 to server }
      val d2 = on[Client] sbj { implicit! => v: Remote[Server] => c0 to server }

      d1.staticAssertType[Int on Client]
      d2.staticAssertType[Int per Server on Client]


      val e0: Unit per Server on Client = placed { implicit! => v: Remote[Server] => d0 }
      val e1 = on[Client] { implicit! => d0 }
      val e2 = on[Client] sbj { implicit! => v: Remote[Server] => d0 }

      e1.staticAssertType[Unit on Client]
      e2.staticAssertType[Unit per Server on Client]


      val f0: Option[Int] on Client = placed { implicit! => Option(c0) }
      val f1 = on[Client] { implicit! => Option(c0) }
      val f2 = on[Client] sbj { implicit! => v: Remote[Server] => Option(c0) }

      f1.staticAssertType[Option[Int] on Client]
      f2.staticAssertType[Option[Int] per Server on Client]


      val g0: Nothing on Client = placed { implicit! => ??? }
      val g1 = on[Client] { implicit! => ??? }
      val g2 = on[Client] { implicit! => g0 }
      val g3 = on[Client] sbj { implicit! => v: Remote[Server] => ??? }
      val g4 = on[Client] sbj { implicit! => v: Remote[Server] => g0 }

      g1.staticAssertType[Nothing on Client]
      g2.staticAssertType[Nothing on Client]
      g3.staticAssertType[Nothing per Server on Client]
      g4.staticAssertType[Nothing per Server on Client]


      val h0: Local[Nothing] on Client = placed { implicit! => ??? }
      val h1 = on[Client] local { implicit! => ??? }
      val h2 = on[Client] local { implicit! => h0 }
      val h3 = on[Client] sbj { implicit! => v: Remote[Server] => ??? }
      val h4 = on[Client] sbj { implicit! => v: Remote[Server] => h0 }

      h1.staticAssertType[Local[Nothing] on Client]
      h2.staticAssertType[Local[Nothing] on Client]
      h3.staticAssertType[Nothing per Server on Client]
      h4.staticAssertType[Nothing per Server on Client]


      val i0: Any on Client = placed { implicit! => 42: Any }
      val i1 = on[Client] { implicit! => 42: Any }
      val i2 = on[Client] { implicit! => i0 }
      val i3 = on[Client] sbj { implicit! => v: Remote[Server] => 42: Any }
      val i4 = on[Client] sbj { implicit! => v: Remote[Server] => i0 }

      i1.staticAssertType[Any on Client]
      i2.staticAssertType[Any on Client]
      i3.staticAssertType[Any per Server on Client]
      i4.staticAssertType[Any per Server on Client]


      val j0: Local[Any] on Client = placed { implicit! => 42: Any }
      val j1 = on[Client] local { implicit! => 42: Any }
      val j2 = on[Client] local { implicit! => j0 }
      val j3 = on[Client] sbj { implicit! => v: Remote[Server] => 42: Any }
      val j4 = on[Client] sbj { implicit! => v: Remote[Server] => j0 }

      j1.staticAssertType[Local[Any] on Client]
      j2.staticAssertType[Local[Any] on Client]
      j3.staticAssertType[Any per Server on Client]
      j4.staticAssertType[Any per Server on Client]


      val k0 = on[Server] { implicit! => remote call c0 }
      val k1 = on[Server] { implicit! => remote call d0 }
      val k2 = on[Server] { implicit! => remote[MobileClient] call c0 }
      val k3 = on[Server] { implicit! => remote(client) call c0 }
      val k4 = on[Server] { implicit! => remote(client, client) call c0 }

      k0.staticAssertType[Unit on Server]
      k1.staticAssertType[Unit on Server]
      k2.staticAssertType[Unit on Server]
      k3.staticAssertType[Unit on Server]
      k4.staticAssertType[Unit on Server]


      val l0 = on[Server] { implicit! => (remote call c0).asLocalFromAll }
      val l1 = on[Server] { implicit! => (remote call d0).asLocalFromAll }
      val l2 = on[Server] { implicit! => (remote[MobileClient] call c0).asLocal }
      val l3 = on[Server] { implicit! => (remote(client) call c0).asLocal }
      val l4 = on[Server] { implicit! => (remote(client, client) call c0).asLocalFromAll }

      l0.staticAssertType[Seq[Future[Int]] on Server]
      l1.staticAssertType[Seq[Future[Int]] on Server]
      l2.staticAssertType[Option[Future[Int]] on Server]
      l3.staticAssertType[Future[Int] on Server]
      l4.staticAssertType[Seq[Future[Int]] on Server]


      on[Server] { implicit! =>
        val a = c0.asLocalFromAll
        a.staticAssertType[Seq[Future[Int]]]

        val b = d0.asLocalFromAll
        b.staticAssertType[Seq[Future[Int]]]

        val c = d0.from[MobileClient].asLocal
        c.staticAssertType[Option[Future[Int]]]

        val d = (d0 from client).asLocal
        d.staticAssertType[Future[Int]]

        val e = (d0 from (client, client)).asLocalFromAll
        e.staticAssertType[Seq[Future[Int]]]

        // `b0.asLocal` should not work since `b0` is local
        // this, however, is only checked during macro expansion
        val f = b0.asLocalFromAll
        f.staticAssertType[Seq[Future[Int]]]

        val g = g0.asLocalFromAll
        g.staticAssertType[Seq[Future[Nothing]]]

        val h = g4.asLocalFromAll
        h.staticAssertType[Seq[Future[Nothing]]]

        val i = g4.from[MobileClient].asLocal
        i.staticAssertType[Option[Future[Nothing]]]

        val j = (g4 from client).asLocal
        j.staticAssertType[Future[Nothing]]

        val k = (g4 from (client, client)).asLocalFromAll
        k.staticAssertType[Seq[Future[Nothing]]]

        // `h0.asLocal` should not work since `h0` is local
        // this, however, is only checked during macro expansion
        val l = h0.asLocalFromAll
        l.staticAssertType[Seq[Future[Nothing]]]
      }


      on[Client] { implicit! =>
        val a = a0.asLocal
        a.staticAssertType[Future[Int]]

        val b = a0.from[SpecialServer].asLocal
        b.staticAssertType[Future[Int]]

        val c = (a0 from server).asLocal
        c.staticAssertType[Future[Int]]

        val d = (a0 from (server, server)).asLocalFromAll
        d.staticAssertType[Seq[Future[Int]]]
      }


      on[Client] { implicit! =>
        // access from the client to the client should not work for the specified architecture
        // this, however, is only checked during macro expansion

        val a = on(client).run { implicit! => 42 }
        a.staticAssertType[Int fromSingle Client]

        val b = on(server, server).run { implicit! => 42 }
        b.staticAssertType[Int fromMultiple Server]

        val c = on[Server].run { implicit! => 42 }
        c.staticAssertType[Int from Server]

        val d = on[Server].run.capture(server) { implicit! => 42 }
        d.staticAssertType[Int from Server]

        // remote blocks with a subjective value for a peer different from the current peer should not work
        // this, however, is only checked during macro expansion

        val e = on(server).run sbj { implicit! => v: Remote[Server] => 42 }
        e.staticAssertType[Int per Server fromSingle Server]

        val f = on(server, server).run sbj { implicit! => v: Remote[Server] => 42 }
        f.staticAssertType[Int per Server fromMultiple Server]

        val g = on[Server].run sbj { implicit! => v: Remote[Server] => 42 }
        g.staticAssertType[Int per Server from Server]

        val h = on[Server].run.capture(server) sbj { implicit! => v: Remote[Server] => 42 }
        h.staticAssertType[Int per Server from Server]
      }
    }""" should compile
  }
}
