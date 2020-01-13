package loci
package language
package impl

import loci.Testing._
import org.scalatest._

import scala.collection.mutable


@multitier object ObjectModule {
  @peer type Peer

  val v = on[loci.language.impl.ObjectModule.Peer] { implicit! => 42 }

  object a {
    object b {
      val y = 0
      object o { val z = loci.language.impl.ObjectModule.a.b.y }
      class C { val z = a.b.y }
    }
  }
}

@multitier trait TopModule {
  val top = 2
}

class TopClass


@multitier object ClassLifting extends TopModule {
  class C(i: Int) { def v = i }
  class D(i: Int) { def v = i * top }

  object d { def v = 4 }

  object e { val v = 8 }

  object o {
    class C(i: Int) { def v = i }
    class D(i: Int) { def v = i * d.v }
  }

  object p {
    class C(i: Int) { def v = i }
    class D(i: Int) { def v = i * d.v }
    class E(i: Int) { def v = i * e.v }
  }

  val mc = new C(2)
  val md = new D(2)
  val oc = new o.C(2)
  val od = new o.D(2)
  val pc = new p.C(2)
  val pd = new p.D(2)
  val pe = new p.E(2)
}

@multitier object TraitLifting extends TopModule {
  trait T { val j: Int }
  class C(i: Int) extends T { val j = 2; def v = j * i }

  trait U { val j = top }
  trait V extends U { def v = j * top }

  val t = new T { val j = 2 }
  val c = new C(4)
  val u = new U { }
  val v = new V { }
}


@multitier object NestedMultitierModules {
  @multitier trait T

  @multitier class C

  @multitier trait A {
    @peer type Peer
    object t extends T
    val c = new C
    val i = on[Peer] { implicit! => 16 }
  }

  @multitier object p {
    import a.{Peer => Test}

    @peer type Peer <: Test with b.Peer

    @multitier object a extends A
    @multitier object b extends A {
      def create = new b
    }
    class b private
  }

  @multitier object q {
    @peer type Peer <: b.Peer

    @multitier object a extends A
    @multitier object b extends A
  }
}


@multitier final class PeerInstantiation(
    result0: => mutable.ListBuffer[String],
    result1: => mutable.ListBuffer[Int],
    result2: => mutable.ListBuffer[Int],
    result3: => mutable.ListBuffer[Int]) {
  @peer type Peer <: o.Peer

  def main(): Unit = result0 += "mod"

  @multitier trait T {
    var variable: Int
  }

  @multitier object o extends T {
    @peer type Peer <: o.InnerPeer0

    def main(): Unit = result0 += "mod.o"

    result1 += variable
    result1 += lazyvalue
    result1 += concreteValue
    result1 += abstractValue0
    result1 += abstractValue1

    override var variable = 42

    lazy val lazyvalue = 1

    val concreteValue = 1

    val abstractValue0: Int

    val abstractValue1: Int

    @multitier object o {
      @peer type InnerPeer0
      @peer type InnerPeer1

      def main(): Unit = result0 += "mod.o.o"

      val innerValue0: Int on InnerPeer0
      val innerValue1: Int on InnerPeer1

      @multitier object p {
        def main(): Unit = result0 += "mod.o.o.p"

        val x: Int
      }

      p
    }

    o

    result2 += variable
    result2 += lazyvalue
    result2 += concreteValue
    result2 += abstractValue0
    result2 += abstractValue1
  }

  o

  result3 += o.variable
  result3 += o.lazyvalue
  result3 += o.concreteValue
  result3 += o.abstractValue0
  result3 += o.abstractValue1
}


@multitier trait SuperModule {
  @peer type A
  @peer type B <: A

  val buffer = on[A] local { implicit! => mutable.ListBuffer.empty[String] }

  on[A] { implicit! => buffer += "super a.0" }
  on[B] { implicit! => buffer += "super b.0" }
  on[A] { implicit! => buffer += "super a.1" }
  on[B] { implicit! => buffer += "super b.1" }
}

@multitier object SubModule extends SuperModule {
  on[A] { implicit! => buffer += "sub a.0" }
  on[B] { implicit! => buffer += "sub b.0" }
  on[A] { implicit! => buffer += "sub a.1" }
  on[B] { implicit! => buffer += "sub b.1" }
}


class MacroExpansionSpec extends FlatSpec with Matchers with NoLogging {
  behavior of "Macro Expansion"

  implicit class StaticTypeAssertion[T](v: T) {
    def staticAssertType[U](implicit ev: T =:= U): Unit = locally(ev)
  }

  def emptySystem(peer: runtime.PlacedValues) = new runtime.System(
    peer,
    None,
    false,
    Map.empty,
    contexts.Immediate.global,
    new runtime.RemoteConnections(
      runtime.Peer.Signature("", List.empty, runtime.Module.Signature("", List.empty)),
      Map.empty),
    Seq.empty,
    Seq.empty)

  it should "typecheck nested objects of the name as the module" in {
    """@multitier trait mod {
      object mod {
        case class mod()
      }

      mod.mod().staticAssertType[mod.mod]
    }""" should compile

    """@multitier trait mod {
      object mod {
        class mod
        def y = new mod
      }

      mod.y.staticAssertType[mod.mod]
    }""" should compile

    """@multitier trait mod {
      val x = 1
      class C { def y = x }
      object mod {
        val x = new C
      }

      mod.x.staticAssertType[C]
    }""" should compile

    """@multitier trait mod { self =>
      val x = 1
      object mod {
        val x = ""
        val y = self.x
      }

      mod.y.staticAssertType[Int]
    }""" should compile
  }

  it should "typecheck access through stable module path" in {
    "ObjectModule.a.b.o.z.staticAssertType[Int]" should compile
    "(new ObjectModule.a.b.C).z.staticAssertType[Int]" should compile
  }

  it should "typecheck implementations in nested objects" in {
    """@multitier trait mod extends TopModule {
      object o {
        trait T { def x =  top }
        object o {
          trait U { myself => def u: T = new T { }; def x = myself.u }
          def z = top
        }
      }

      def u = new o.o.U { override def u = new o.T { } }

      u.staticAssertType[o.o.U]
      u.u.staticAssertType[o.T]
    }""" should compile

    """@multitier trait mod extends TopModule {
      object o {
        trait T { def x = top }
        object o {
          trait U {
            trait U { def u: T = new T { } }
          }
          def z = top
        }
      }

      val u0 = new o.o.U { }
      val u1 = new u0.U { override def u = new o.T { } }

      u1.staticAssertType[u0.U]
      u1.u.staticAssertType[o.T]
    }""" should compile

    """@multitier trait mod extends TopModule {
      private object o {
        val y = 0

        class C {
          def c = top
          def obj0 = o
          def obj1: o.type = o
        }
        object C

        class D { def d = y }
        object D {
          def d = ""
          def create0(i: Int) = new D { override def d: Int = i }
          def create1(i: Int): D = new D { override def d: Int = i }
          def create2(i: Int): o.D = new D { override def d: Int = i }
         }

        trait T

        case class CC(i: Int) extends T
        object CC {
          def z = ""
        }

        case object co { def z = "" }

        class X { def t = top }

        type Y = mod.this.o.X

        def get = new D

        private object inner
      }

      val x = (null: AnyRef) match {
        case o.CC(i) => i
        case _ => 0
      }

      x.staticAssertType[Int]
      o.y.staticAssertType[Int]
      (new o.C).c.staticAssertType[Int]
      (new o.C).obj0.y.staticAssertType[Int]
      (new o.C).obj1.y.staticAssertType[Int]
      (new o.D).d.staticAssertType[Int]
      o.D.d.staticAssertType[String]
      o.D.create0(0).staticAssertType[o.D]
      o.D.create1(0).staticAssertType[o.D]
      o.D.create2(0).staticAssertType[o.D]
      o.co.z.staticAssertType[String]
      (new o.X).staticAssertType[o.X]
      (new o.X).staticAssertType[o.Y]
      (new o.Y).staticAssertType[o.X]
      (new o.Y).staticAssertType[o.Y]
      o.get.staticAssertType[o.D]
    }""" should compile

    """@multitier trait mod extends TopModule {
      object o {
        trait T { def t = top }
        type X = T
        class C[U](v: U) { def c = top }

        def get0 = new C(new X {})
        def get1 = new C(0) with T { type X }
      }

      def internal() = {
        final class C { def c = top }
        ()
      }

      def get = new o.C(0)

      def set(v: o.C[Int]) = {
        var x = new o.C(0)
        x = v
      }

      (new o.T {}).staticAssertType[o.T]
      (new o.X {}).staticAssertType[o.T]
      (new o.T {}).staticAssertType[o.X]
      (new o.X {}).staticAssertType[o.X]
      (new o.T {}).t.staticAssertType[Int]
      (new o.X {}).t.staticAssertType[Int]
      (new o.C("")).staticAssertType[o.C[String]]
      (new o.C("")).c.staticAssertType[Int]
      o.get0.staticAssertType[o.C[o.X]]
      o.get0.staticAssertType[o.C[o.T]]
      o.get1.staticAssertType[o.C[Int] with o.T { type X }]
      get.staticAssertType[o.C[Int]]
      set(new o.C(0)).staticAssertType[Unit]
    }""" should compile
  }

  it should "typecheck early initializers" in {
    """@multitier trait mod extends TopModule {
      class C[T](v: T) { def z = top; type X = Int }

      def a = new { val x = 1 } with C[Int](0) { self: C[Int] => type Y = Int; trait T { type U }; def u(x: Int)(y: Int) = x + y }

      trait U { def z = top; type X = Int }

      def b = new { val x = 1 } with U { self: U => type Y = Int; trait T { type U }; def u(x: Int)(y: Int) = x + y }

      (new C("")).staticAssertType[C[String]]
      a.staticAssertType[C[Int] { val x: Int; type Y = Int; type T <: { type U }; def u(x: Int)(y: Int): Int}]
      b.staticAssertType[U { val x: Int; type Y = Int; type T <: { type U }; def u(x: Int)(y: Int): Int}]
    }""" should compile
  }

  it should "typecheck nested types" in {
    """@multitier trait mod extends TopModule {
      class B

      class C(s: String) {
        def this(i: Int) = this(i.toString)
        def x = top
      }

      trait T {
        def y = top
      }

      class Liftable

      class D extends T

      type AliasC = C

      type AliasLiftable = Liftable

      object o {
        class X extends mod.this.C(0)
        type AliasX = X

        class Liftable
        type AliasLiftable = Liftable

        def v = 1

        object o {
          class X
        }
      }

      class E extends C("")

      val a = new Liftable
      val b = new C(0)
      val c = new C(0) with T { val v = 1 }
      val d = new AliasC(0) with T { }
      val e = new o.AliasLiftable with T { }
      val f = new o.AliasX
      val g = new TopClass { }
      val h = new TopClass

      a.staticAssertType[Liftable]
      b.staticAssertType[C]
      c.staticAssertType[C with T { val v: Int }]
      d.staticAssertType[AliasC with T]
      e.staticAssertType[o.AliasLiftable with T]
      f.staticAssertType[o.AliasX]
      g.staticAssertType[TopClass]
      h.staticAssertType[TopClass]
    }""" should compile
  }

  it should "correctly lift types" in {
    val placedValuesClasses = new ClassLifting.`<placed values of loci.language.impl.ClassLifting>` {
      lazy val $loci$sig = ""
      def $loci$sys$create = emptySystem(this)
    }

    placedValuesClasses.mc.v should be (2)
    placedValuesClasses.md.v should be (4)
    placedValuesClasses.oc.v should be (2)
    placedValuesClasses.od.v should be (8)
    placedValuesClasses.pc.v should be (2)
    placedValuesClasses.pd.v should be (8)
    placedValuesClasses.pe.v should be (16)

    ClassLifting.d should be (placedValuesClasses.d)
    ClassLifting.o should be (placedValuesClasses.o)
    ClassLifting.e shouldNot be (placedValuesClasses.e)
    ClassLifting.p shouldNot be (placedValuesClasses.p)

    new ClassLifting.C(2).v should be (2)
    new ClassLifting.o.C(2).v should be (2)
    new ClassLifting.p.C(2).v should be (2)

    // untypeability not correctly detected by the testing framework
    // since `compileTimeOnly` annotation is checked later during compilation
    //
    // "new ClassLifting.D(2)" shouldNot typeCheck
    // "new ClassLifting.o.D(2)" shouldNot typeCheck
    // "new ClassLifting.p.D(2)" shouldNot typeCheck
    // "new ClassLifting.p.E(2)" shouldNot typeCheck


    val placedValuesTraits = new TraitLifting.`<placed values of loci.language.impl.TraitLifting>` {
      lazy val $loci$sig = ""
      def $loci$sys$create = emptySystem(this)
    }

    placedValuesTraits.t.j should be (2)
    placedValuesTraits.c.v should be (8)
    placedValuesTraits.u.j should be (2)
    placedValuesTraits.v.v should be (4)

    new TraitLifting.T { val j = 2 }.j should be (2)
    new TraitLifting.C(4).v should be (8)

    // untypeability not correctly detected by the testing framework
    // since `compileTimeOnly` annotation is checked later during compilation
    //
    // "new TraitLifting.U { }" shouldNot typeCheck
    // "new TraitLifting.V { }" shouldNot typeCheck
  }

  it should "typecheck nested multitier modules" in {
    """@multitier object mod {
      @multitier trait A {
        val a: Int = 4
      }

      @multitier trait B {
        val m: A
        val b: Int = m.a
      }

      @multitier object o extends B {
        @multitier trait T

        @multitier object m extends A with T {
          val c = 8
          val d = o.m.c

          @multitier trait T { val x = "" }
        }

        object n {
          @multitier trait T { val w = "" }
        }

        val e = m.d
      }
    }""" should compile
  }

  it should "typecheck default arguments" in {
    """@multitier object mod {
      val x = 1

      def f(x: Int = 1) = ()

      class C(x: Int = 1) {
        def f(x: Int = 1) = ()
      }

      object o {
        val y = x

        def f(x: Int = 1) = ()

        f()
        new C()
        mod.this.f()
        mod.this.o.f()
        new mod.this.C()
        mod.f()
        mod.o.f()
        new mod.C()
      }

      f()
      o.f()
      new C()
      mod.this.f()
      mod.this.o.f()
      new mod.this.C()
      mod.f()
      mod.o.f()
      new mod.C()
    }""" should compile
  }

  it should "correctly compile nested multitier modules" in {
    new NestedMultitierModules.p.$loci$peer$Peer {
      def $loci$sys$create = emptySystem(this)
    }.a.i should be (16)

    new NestedMultitierModules.p.$loci$peer$Peer {
      def $loci$sys$create = emptySystem(this)
    }.b.i should be (16)

    new NestedMultitierModules.q.$loci$peer$Peer {
      def $loci$sys$create = emptySystem(this)
    }.a.i should be (0)

    new NestedMultitierModules.q.$loci$peer$Peer {
      def $loci$sys$create = emptySystem(this)
    }.b.i should be (16)
  }

  it should "correctly compile derived multitier modules" in {
    new SubModule.`<placed values of loci.language.impl.SubModule>` {
      def $loci$sys$create = emptySystem(this)
    }.buffer should be (null)

    new SubModule.$loci$peer$A {
      def $loci$sys$create = emptySystem(this)
    }.buffer should contain theSameElementsInOrderAs Seq(
      "super a.0", "super a.1", "sub a.0", "sub a.1")

    new SubModule.$loci$peer$B {
      def $loci$sys$create = emptySystem(this)
    }.buffer should contain theSameElementsInOrderAs Seq(
      "super a.0", "super b.0", "super a.1", "super b.1", "sub a.0", "sub b.0", "sub a.1","sub b.1")
  }

  it should "correctly instantiate peer " in {
    val result0 = mutable.ListBuffer.empty[String]
    val result1 = mutable.ListBuffer.empty[Int]
    val result2 = mutable.ListBuffer.empty[Int]
    val result3 = mutable.ListBuffer.empty[Int]

    val mod = new PeerInstantiation(result0, result1, result2, result3)

    multitier start new loci.Instance[mod.Peer](contexts.Immediate.global, separateMainThread = false) {
      @multitier object o {
        override lazy val lazyvalue = 42

        override val concreteValue = 42

        val abstractValue0 = 42

        val abstractValue1 = localDefinition

        def localDefinition = 42

        @multitier object o {
          val innerValue0 = 1

          @multitier object p {
            val x = 1
          }
        }
      }
    }

    result0 should contain theSameElementsAs Seq("mod")
    result1 should contain theSameElementsAs Seq(0, 42, 42, 42, 0)
    result2 should contain theSameElementsAs Seq(42, 42, 42, 42, 0)
    result3 should contain theSameElementsAs Seq(42, 42, 42, 42, 42)
  }
}
