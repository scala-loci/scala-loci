package loci
package embedding

import language._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.Future

class TypeLevelEncodingSpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "Type Level Encoding"

  it should "typecheck remote access" in {
    CompileTimeUtils.assertNoFailedAssertion(CompileTimeUtils.compile(CompileTimeUtils.replace(CompileTimeUtils.replace(
      """trait Module {
        @peer type Component
        @peer type Server <: Component { type Tie <: Multiple[Client] with Optional[MobileClient] }
        @peer type Client <: Component { type Tie <: Single[Server] with Single[SpecialServer] }
        @peer type MobileClient <: Client { type Tie <: Single[Server] with Single[SpecialServer] }
        @peer type SpecialServer <: Server { type Tie <: Multiple[Client] with Optional[MobileClient] }


        val server: Remote[Server] = ???
        val client: Remote[Client] = ???


        val a0: Int on Server = placed { implicit! => 42 }
        val a1 = on[Server] { implicit! => a0 to server }
        val a2 = on[Server] sbj { implicit! => v: Remote[Client] => a0 to server }

        CompileTimeUtils.assertExactType[Int on Server](a1)
        CompileTimeUtils.assertExactType[Int per Client on Server](a2)


        val b0: Local[Int] on Client = placed { implicit! => 42 }
        val b1 = on[Client] local { implicit! => b0 to server }
        val b2 = on[Client] sbj { implicit! => v: Remote[Server] => b0 to server }

        CompileTimeUtils.assertExactType[Local[Int] on Client](b1)
        CompileTimeUtils.assertExactType[Int per Server on Client](b2)


        val c0: Int on Client = placed { implicit! => b0 }
        val c1 = on[Client] { implicit! => b0 }
        val c2 = on[Client] sbj { implicit! => v: Remote[Server] => b0 }

        CompileTimeUtils.assertExactType[Int on Client](c1)
        CompileTimeUtils.assertExactType[Int per Server on Client](c2)


        val d0: Int per Server on Client = placed { implicit! => v: Remote[Server] => 42 }
        val d1 = on[Client] { implicit! => c0 to server }
        val d2 = on[Client] sbj { implicit! => v: Remote[Server] => c0 to server }

        CompileTimeUtils.assertExactType[Int on Client](d1)
        CompileTimeUtils.assertExactType[Int per Server on Client](d2)


        val e0: Unit per Server on Client = placed { implicit! => v: Remote[Server] => d0 }
        val e1 = on[Client] { implicit! => d0 }
        val e2 = on[Client] sbj { implicit! => v: Remote[Server] => d0 }

        CompileTimeUtils.assertExactType[Unit on Client](e1)
        CompileTimeUtils.assertExactType[Unit per Server on Client](e2)


        val f0: Option[Int] on Client = placed { implicit! => Option(c0) }
        val f1 = on[Client] { implicit! => Option(c0) }
        val f2 = on[Client] sbj { implicit! => v: Remote[Server] => Option(c0) }

        CompileTimeUtils.assertExactType[Option[Int] on Client](f1)
        CompileTimeUtils.assertExactType[Option[Int] per Server on Client](f2)


        val g0: Nothing on Client = placed { implicit! => ??? }
        val g1 = on[Client] { implicit! => ??? }
        val g2 = on[Client] { implicit! => g0 }
        val g3 = on[Client] sbj { implicit! => v: Remote[Server] => ??? }
        val g4 = on[Client] sbj { implicit! => v: Remote[Server] => g0 }

        CompileTimeUtils.assertExactType[Nothing on Client](g1)
        CompileTimeUtils.assertExactType[Nothing on Client](g2)
        CompileTimeUtils.assertExactType[Nothing per Server on Client](g3)
        CompileTimeUtils.assertExactType[Nothing per Server on Client](g4)


        val h0: Local[Nothing] on Client = placed { implicit! => ??? }
        val h1 = on[Client] local { implicit! => ??? }
        val h2 = on[Client] local { implicit! => h0 }
        val h3 = on[Client] sbj { implicit! => v: Remote[Server] => ??? }
        val h4 = on[Client] sbj { implicit! => v: Remote[Server] => h0 }

        CompileTimeUtils.assertExactType[Local[Nothing] on Client](h1)
        CompileTimeUtils.assertExactType[Local[Nothing] on Client](h2)
        CompileTimeUtils.assertExactType[Nothing per Server on Client](h3)
        CompileTimeUtils.assertExactType[Nothing per Server on Client](h4)


        val i0: Any on Client = placed { implicit! => 42: Any }
        val i1 = on[Client] { implicit! => 42: Any }
        val i2 = on[Client] { implicit! => i0 }
        val i3 = on[Client] sbj { implicit! => v: Remote[Server] => 42: Any }
        val i4 = on[Client] sbj { implicit! => v: Remote[Server] => i0 }

        CompileTimeUtils.assertExactType[Any on Client](i1)
        CompileTimeUtils.assertExactType[Any on Client](i2)
        CompileTimeUtils.assertExactType[Any per Server on Client](i3)
        CompileTimeUtils.assertExactType[Any per Server on Client](i4)


        val j0: Local[Any] on Client = placed { implicit! => 42: Any }
        val j1 = on[Client] local { implicit! => 42: Any }
        val j2 = on[Client] local { implicit! => j0 }
        val j3 = on[Client] sbj { implicit! => v: Remote[Server] => 42: Any }
        val j4 = on[Client] sbj { implicit! => v: Remote[Server] => j0 }

        CompileTimeUtils.assertExactType[Local[Any] on Client](j1)
        CompileTimeUtils.assertExactType[Local[Any] on Client](j2)
        CompileTimeUtils.assertExactType[Any per Server on Client](j3)
        CompileTimeUtils.assertExactType[Any per Server on Client](j4)


        val k0 = on[Server] { implicit! => remote call c0 }
        val k1 = on[Server] { implicit! => remote call d0 }
        val k2 = on[Server] { implicit! => remote[MobileClient] call c0 }
        val k3 = on[Server] { implicit! => remote(client) call c0 }
        val k4 = on[Server] { implicit! => remote(client, client) call c0 }

        CompileTimeUtils.assertExactType[Unit on Server](k0)
        CompileTimeUtils.assertExactType[Unit on Server](k1)
        CompileTimeUtils.assertExactType[Unit on Server](k2)
        CompileTimeUtils.assertExactType[Unit on Server](k3)
        CompileTimeUtils.assertExactType[Unit on Server](k4)


        val l0 = on[Server] { implicit! => (remote call c0).asLocalFromAll }
        val l1 = on[Server] { implicit! => (remote call d0).asLocalFromAll }
        val l2 = on[Server] { implicit! => (remote[MobileClient] call c0).asLocal }
        val l3 = on[Server] { implicit! => (remote(client) call c0).asLocal }
        val l4 = on[Server] { implicit! => (remote(client, client) call c0).asLocalFromAll }

        CompileTimeUtils.assertExactType[Seq[(Remote[Client], Future[Int])] on Server](l0)
        CompileTimeUtils.assertExactType[Seq[(Remote[Client], Future[Int])] on Server](l1)
        CompileTimeUtils.assertExactType[Option[Future[Int]] on Server](l2)
        CompileTimeUtils.assertExactType[Future[Int] on Server](l3)
        CompileTimeUtils.assertExactType[Seq[(Remote[Client], Future[Int])] on Server](l4)


        on[Server] { implicit! =>
          val a = c0.asLocalFromAll
          CompileTimeUtils.assertExactType[Seq[(Remote[Client], Future[Int])]](a)

          val b = d0.asLocalFromAll
          CompileTimeUtils.assertExactType[Seq[(Remote[Client], Future[Int])]](b)

          val c = d0.from[MobileClient].asLocal
          CompileTimeUtils.assertExactType[Option[Future[Int]]](c)

          val d = (d0 from client).asLocal
          CompileTimeUtils.assertExactType[Future[Int]](d)

          val e = (d0 from (client, client)).asLocalFromAll
          CompileTimeUtils.assertExactType[Seq[(Remote[Client], Future[Int])]](e)

          // `b0.asLocalFromAll` should not work since `b0` is local
          // this, however, is only checked during macro expansion
          val f = b0.asLocalFromAll
          CompileTimeUtils.assertExactType[Seq[(Remote[Client], Future[Int])]](f)

          val g = g0.asLocalFromAll
          CompileTimeUtils.assertExactType[Seq[(Remote[Client], Future[Nothing])]](g)

          val h = g4.asLocalFromAll
          CompileTimeUtils.assertExactType[Seq[(Remote[Client], Future[Nothing])]](h)

          val i = g4.from[MobileClient].asLocal
          CompileTimeUtils.assertExactType[Option[Future[Nothing]]](i)

          val j = (g4 from client).asLocal
          CompileTimeUtils.assertExactType[Future[Nothing]](j)

          val k = (g4 from (client, client)).asLocalFromAll
          CompileTimeUtils.assertExactType[Seq[(Remote[Client], Future[Nothing])]](k)

          // `h0.asLocalFromAll` should not work since `h0` is local
          // this, however, is only checked during macro expansion
          val l = h0.asLocalFromAll
          CompileTimeUtils.assertExactType[Seq[(Remote[Client], Future[Nothing])]](l)
        }


        on[Client] { implicit! =>
          val a = a0.asLocal
          CompileTimeUtils.assertExactType[Future[Int]](a)

          val b = a0.from[SpecialServer].asLocal
          CompileTimeUtils.assertExactType[Future[Int]](b)

          val c = (a0 from server).asLocal
          CompileTimeUtils.assertExactType[Future[Int]](c)

          val d = (a0 from (server, server)).asLocalFromAll
          CompileTimeUtils.assertExactType[Seq[(Remote[Server], Future[Int])]](d)
        }


        on[Client] { implicit! =>
          val value = 42

          // access from the client to the client should not work for the specified architecture
          // this, however, is only checked during macro expansion
          val a = on(client).run { implicit! => 42 }
          CompileTimeUtils.assertType[Int fromSingle Client](a)

          val b = on(server, server).run { implicit! => 42 }
          CompileTimeUtils.assertType[Int fromMultiple Server](b)

          val c = on[Server].run { implicit! => 42 }
          CompileTimeUtils.assertType[Int from Server](c)

          val d = on[Server].run.capture(value) { implicit! => 42 }
          CompileTimeUtils.assertType[Int from Server](d)

          // remote blocks with a subjective value for a peer different from the current peer should not work
          // this, however, is only checked during macro expansion
          val e = on(server).run sbj { implicit! => v: Remote[Server] => 42 }
          CompileTimeUtils.assertType[Int per Server fromSingle Server](e)

          val f = on(server, server).run sbj { implicit! => v: Remote[Client] => 42 }
          CompileTimeUtils.assertType[Int per Client fromMultiple Server](f)

          val g = on[Server].run sbj { implicit! => v: Remote[Client] => 42 }
          CompileTimeUtils.assertType[Int per Client from Server](g)

          val h = on[Server].run.capture(value) sbj { implicit! => v: Remote[Client] => 42 }
          CompileTimeUtils.assertType[Int per Client from Server](h)
        }
      }""",
      "from (client, client)", "from ((client, client))"),
      "from (server, server)", "from ((server, server))")))
  }
}
