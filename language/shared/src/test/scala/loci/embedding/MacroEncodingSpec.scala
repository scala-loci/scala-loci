package loci
package embedding

import language._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.Future

class MacroEncodingSpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "Macro Encoding"

  it should "typecheck remote access" in {
    CompileTimeUtils.assertNoFailedAssertion("""@multitier trait Module {
      @peer type Component
      @peer type Server <: Component { type Tie <: Multiple[Client] with Optional[MobileClient] }
      @peer type Client <: Component { type Tie <: Single[Server] with Single[SpecialServer] }
      @peer type MobileClient <: Client { type Tie <: Single[Server] with Single[SpecialServer] }
      @peer type SpecialServer <: Server { type Tie <: Multiple[Client] with Optional[MobileClient] }


      val server: Remote[Server] = ???
      val client: Remote[Client] = ???


      val a0: Int on Server = 42
      val a1 = on[Server] { a0 to server }
      val a2 = on[Server] sbj { v: Remote[Client] => a0 to server }

      CompileTimeUtils.assertExactType[Int on Server](a1)
      CompileTimeUtils.assertExactType[Int per Client on Server](a2)


      val b0: Local[Int] on Client = 42
      val b1 = on[Client] local { b0 to server }
      val b2 = on[Client] sbj { v: Remote[Server] => b0 to server }

      CompileTimeUtils.assertExactType[Local[Int] on Client](b1)
      CompileTimeUtils.assertExactType[Int per Server on Client](b2)


      val c0: Int on Client = b0
      val c1 = on[Client] { b0 }
      val c2 = on[Client] sbj { v: Remote[Server] => b0 }

      CompileTimeUtils.assertExactType[Int on Client](c1)
      CompileTimeUtils.assertExactType[Int per Server on Client](c2)


      val d0: Int per Server on Client = { v: Remote[Server] => 42 }
      val d1 = on[Client] { c0 to server }
      val d2 = on[Client] sbj { v: Remote[Server] => c0 to server }

      CompileTimeUtils.assertExactType[Int on Client](d1)
      CompileTimeUtils.assertExactType[Int per Server on Client](d2)


      val e0: Unit per Server on Client = { v: Remote[Server] => locally(d0) }
      val e1 = on[Client] { locally(d0) }
      val e2 = on[Client] sbj { v: Remote[Server] => locally(d0) }

      CompileTimeUtils.assertExactType[Unit on Client](e1)
      CompileTimeUtils.assertExactType[Unit per Server on Client](e2)


      val f0: Option[Int] on Client = Option(c0)
      val f1 = on[Client] { Option(c0) }
      val f2 = on[Client] sbj { v: Remote[Server] => Option(c0) }

      CompileTimeUtils.assertExactType[Option[Int] on Client](f1)
      CompileTimeUtils.assertExactType[Option[Int] per Server on Client](f2)


      val g0: Nothing on Client = ???
      val g1 = on[Client] { ??? }
      val g2 = on[Client] { g0 }
      val g3 = on[Client] sbj { v: Remote[Server] => ??? }
      val g4 = on[Client] sbj { v: Remote[Server] => g0 }

      CompileTimeUtils.assertExactType[Nothing on Client](g1)
      CompileTimeUtils.assertExactType[Nothing on Client](g2)
      CompileTimeUtils.assertExactType[Nothing per Server on Client](g3)
      CompileTimeUtils.assertExactType[Nothing per Server on Client](g4)


      val h0: Local[Nothing] on Client = ???
      val h1 = on[Client] local { ??? }
      val h2 = on[Client] local { h0 }
      val h3 = on[Client] sbj { v: Remote[Server] => ??? }
      val h4 = on[Client] sbj { v: Remote[Server] => h0 }

      CompileTimeUtils.assertExactType[Local[Nothing] on Client](h1)
      CompileTimeUtils.assertExactType[Local[Nothing] on Client](h2)
      CompileTimeUtils.assertExactType[Nothing per Server on Client](h3)
      CompileTimeUtils.assertExactType[Nothing per Server on Client](h4)


      val i0: Any on Client = 42: Any
      val i1 = on[Client] { 42: Any }
      val i2 = on[Client] { i0 }
      val i3 = on[Client] sbj { v: Remote[Server] => 42: Any }
      val i4 = on[Client] sbj { v: Remote[Server] => i0 }

      CompileTimeUtils.assertExactType[Any on Client](i1)
      CompileTimeUtils.assertExactType[Any on Client](i2)
      CompileTimeUtils.assertExactType[Any per Server on Client](i3)
      CompileTimeUtils.assertExactType[Any per Server on Client](i4)


      val j0: Local[Any] on Client = 42: Any
      val j1 = on[Client] local { 42: Any }
      val j2 = on[Client] local { j0 }
      val j3 = on[Client] sbj { v: Remote[Server] => 42: Any }
      val j4 = on[Client] sbj { v: Remote[Server] => j0 }

      CompileTimeUtils.assertExactType[Local[Any] on Client](j1)
      CompileTimeUtils.assertExactType[Local[Any] on Client](j2)
      CompileTimeUtils.assertExactType[Any per Server on Client](j3)
      CompileTimeUtils.assertExactType[Any per Server on Client](j4)


      val k0 = on[Server] { remote call c0 }
      val k1 = on[Server] { remote call d0 }
      val k2 = on[Server] { remote[MobileClient] call c0 }
      val k3 = on[Server] { remote(client) call c0 }
      val k4 = on[Server] { remote(client, client) call c0 }

      CompileTimeUtils.assertExactType[Unit on Server](k0)
      CompileTimeUtils.assertExactType[Unit on Server](k1)
      CompileTimeUtils.assertExactType[Unit on Server](k2)
      CompileTimeUtils.assertExactType[Unit on Server](k3)
      CompileTimeUtils.assertExactType[Unit on Server](k4)


      val l0 = on[Server] { (remote call c0).asLocalFromAll }
      val l1 = on[Server] { (remote call d0).asLocalFromAll }
      val l2 = on[Server] { (remote[MobileClient] call c0).asLocal }
      val l3 = on[Server] { (remote(client) call c0).asLocal }
      val l4 = on[Server] { (remote(client, client) call c0).asLocalFromAll }

      CompileTimeUtils.assertExactType[Seq[(Remote[Client], Future[Int])] on Server](l0)
      CompileTimeUtils.assertExactType[Seq[(Remote[Client], Future[Int])] on Server](l1)
      CompileTimeUtils.assertExactType[Option[Future[Int]] on Server](l2)
      CompileTimeUtils.assertExactType[Future[Int] on Server](l3)
      CompileTimeUtils.assertExactType[Seq[(Remote[Client], Future[Int])] on Server](l4)


      on[Server] {
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
      }


      on[Client] {
        val a = a0.asLocal
        CompileTimeUtils.assertExactType[Future[Int]](a)

        val b = a0.from[SpecialServer].asLocal
        CompileTimeUtils.assertExactType[Future[Int]](b)

        val c = (a0 from server).asLocal
        CompileTimeUtils.assertExactType[Future[Int]](c)

        val d = (a0 from (server, server)).asLocalFromAll
        CompileTimeUtils.assertExactType[Seq[(Remote[Server], Future[Int])]](d)
      }


      on[Client] {
        val value = 42

        val b = on(server, server).run { locally(42) }
        CompileTimeUtils.assertType[Int fromMultiple Server](b)

        val c = on[Server].run { locally(42) }
        CompileTimeUtils.assertType[Int from Server](c)

        val d = on[Server].run.capture(value) { locally(42) }
        CompileTimeUtils.assertType[Int from Server](d)

        val f = on(server, server).run sbj { v: Remote[Client] => locally(42) }
        CompileTimeUtils.assertType[Int per Client fromMultiple Server](f)

        val g = on[Server].run sbj { v: Remote[Client] => locally(42) }
        CompileTimeUtils.assertType[Int per Client from Server](g)

        val h = on[Server].run.capture(value) sbj { v: Remote[Client] => locally(42) }
        CompileTimeUtils.assertType[Int per Client from Server](h)
      }
    }""")
  }

  it should "not typecheck remote access" in {
    """@multitier trait Module {
      @peer type Component
      @peer type Server <: Component { type Tie <: Multiple[Client] with Optional[MobileClient] }
      @peer type Client <: Component { type Tie <: Single[Server] with Single[SpecialServer] }
      @peer type MobileClient <: Client { type Tie <: Single[Server] with Single[SpecialServer] }
      @peer type SpecialServer <: Server { type Tie <: Multiple[Client] with Optional[MobileClient] }

      val b0: Local[Int] on Client = 42

      on[Server] {
        val f = b0.asLocalFromAll
      }
    }""" shouldNot typeCheck

    """@multitier trait Module {
      @peer type Component
      @peer type Server <: Component { type Tie <: Multiple[Client] with Optional[MobileClient] }
      @peer type Client <: Component { type Tie <: Single[Server] with Single[SpecialServer] }
      @peer type MobileClient <: Client { type Tie <: Single[Server] with Single[SpecialServer] }
      @peer type SpecialServer <: Server { type Tie <: Multiple[Client] with Optional[MobileClient] }

      val h0: Local[Nothing] on Client = ???

      on[Server] {
        val l = h0.asLocalFromAll
      }
    }""" shouldNot typeCheck

    """@multitier trait Module {
      @peer type Component
      @peer type Server <: Component { type Tie <: Multiple[Client] with Optional[MobileClient] }
      @peer type Client <: Component { type Tie <: Single[Server] with Single[SpecialServer] }
      @peer type MobileClient <: Client { type Tie <: Single[Server] with Single[SpecialServer] }
      @peer type SpecialServer <: Server { type Tie <: Multiple[Client] with Optional[MobileClient] }

      val client: Remote[Client] = ???

      on[Client] {
        val a = on(client).run { 42 }
      }
    }""" shouldNot typeCheck

    """@multitier trait Module {
      @peer type Component
      @peer type Server <: Component { type Tie <: Multiple[Client] with Optional[MobileClient] }
      @peer type Client <: Component { type Tie <: Single[Server] with Single[SpecialServer] }
      @peer type MobileClient <: Client { type Tie <: Single[Server] with Single[SpecialServer] }
      @peer type SpecialServer <: Server { type Tie <: Multiple[Client] with Optional[MobileClient] }

      val server: Remote[Server] = ???

      on[Client] {
        val e = on(server).run sbj { v: Remote[Server] => 42 }
      }
    }""" shouldNot typeCheck
  }
}
