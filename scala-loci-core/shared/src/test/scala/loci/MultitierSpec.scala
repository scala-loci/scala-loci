package loci

import transmitter._
import transmission._
import org.scalatest._

class MultitierSpec extends FlatSpec with Matchers {
  behavior of "Multitier Macro Expansion"

  implicit def dummyMarshallable0: MarshallableArgument[Int] = ???
  implicit def dummyMarshallable1: MarshallableArgument[String] = ???
  implicit def dummyMarshallable2: MarshallableArgument[(Int, Int)] = ???
  implicit def dummyMarshallable3: MarshallableArgument[(Int, String)] = ???
  implicit def dummyMarshallable4: MarshallableArgument[(Double, String)] = ???
  implicit def dummyMarshallable5: MarshallableArgument[(Double, (String, Int))] = ???

  implicit class DummyMultipleTransmissionProvider
      [T, R <: Peer, L <: Peer]
      (transmission: MultipleTransmission[T, R, L])
    extends TransmissionProvider {
    def asLocal: List[T] = ???
  }

  implicit class DummyOptionalTransmissionProvider
      [T, R <: Peer, L <: Peer]
      (transmission: OptionalTransmission[T, R, L])
    extends TransmissionProvider {
    def asLocal: Option[T] = ???
  }

  implicit class DummySingleTransmissionProvider
      [T, R <: Peer, L <: Peer]
      (transmission: SingleTransmission[T, R, L])
    extends TransmissionProvider {
    def asLocal: T = ???
  }

  implicit class StaticTypeAssertion[T](v: T) {
    def staticAssertType[U](implicit ev: T =:= U) = { }
  }

  it should "typecheck local unzipping for connected remote list" in {
    """
    @multitier
    object test {
      trait Registry extends Peer {
        type Tie <: Multiple[Node]
      }
      trait Node extends Peer {
        type Tie <: Multiple[Node] with Optional[Registry]
      }

      placed[Registry] { implicit! =>
        val (nodes, users) =
          (remote[Node].connected map { node =>
            node -> 0
          }).unzip
      }
    }""" should compile
  }

  it should "typecheck partial function literal for collecting" in {
    """
    @multitier
    object test {
      trait Registry extends Peer {
        type Tie <: Multiple[Node]
      }
      trait Node extends Peer {
        type Tie <: Multiple[Node] with Optional[Registry]
      }

      placed[Registry] { implicit! =>
        Seq(Some(0), None) collect { case Some(x) => x }
      }
    }""" should compile
  }

  it should "typecheck nested remote expressions" in {
    """
    @multitier
    object test {
      trait Registry extends Peer {
        type Tie <: Multiple[Node]
      }
      trait Node extends Peer {
        type Tie <: Multiple[Node] with Optional[Registry]
      }

      val remotes = placed[Registry].local { implicit! => Map.empty[Int, Remote[Node]] }

      def update(remoteId: Int, update: String) = placed[Node].local { implicit! =>
        remote[Registry].sbj.capture(remoteId, update) { implicit! => node: Remote[Node] =>
          (remotes get remoteId) foreach { requested =>
            remote.on(requested).capture(update) { implicit! =>
              def fortytwo = 42
              fortytwo
            }
          }
        }
      }
    }""" should compile
  }

  it should "typecheck nested case classes and objects" in {
    """
    @multitier
    object test {
      trait Registry extends Peer {
        type Tie <: Multiple[Node]
      }
      trait Node extends Peer {
        type Tie <: Multiple[Node] with Optional[Registry]
      }

      trait Stuff

      case class Thing(s: Stuff)

      case class Case1(id: Int) extends Stuff
      case class Case2(id: Int, t: Thing, s: String) extends Stuff

      case object Case3 extends Stuff

      object Case1 {
        def test = ""
      }

      def test = placed[Registry].local { implicit! =>
        null.asInstanceOf[Stuff] match {
          case Case1(id) =>
          case Case2(_, Thing(Case2(id, _, "")), s) =>
          case Case3 =>
        }

        Case1.test
        42
      }
    }""" should compile
  }

  it should "use correct types for remote access" in {
    @multitier
    class test {
      trait Registry extends Peer {
        type Tie <: Multiple[Node]
      }
      trait Node extends Peer {
        type Tie <: Multiple[Node] with Optional[Registry]
      }

      val id = placed[Node] { implicit! => 0 }

      val url = placed[Registry] { implicit! => "" }
    }

    """
    @multitier
    object test extends test {
      trait MyNode extends Node
      placed[MyNode] { implicit! => val a: List[Int] = id.asLocal }
    }""" should compile

    """
    @multitier
    object test extends test {
      trait MyNode extends Node
      placed[MyNode] { implicit! => val a: String = id.asLocal }
    }""" shouldNot typeCheck

    """
    @multitier
    object test extends test {
      trait MyNode extends Node
      placed[MyNode] { implicit! => val a: Option[String] = url.asLocal }
    }""" should compile

    """
    @multitier
    object test extends test {
      trait MyNode extends Node
      placed[MyNode] { implicit! => val a: Int = url.asLocal }
    }""" shouldNot typeCheck

    """
    @multitier
    object test extends test {
      trait MyRegistry extends Registry
      placed[MyRegistry] { implicit! => val a: List[Int] = id.asLocal }
    }""" should compile

    """
    @multitier
    object test extends test {
      trait MyRegistry extends Registry
      placed[MyRegistry] { implicit! => val a: String = id.asLocal }
    }""" shouldNot typeCheck

    """
    @multitier
    object test extends test {
      trait MyRegistry extends Registry
      placed[MyRegistry] { implicit! => val a = url.asLocal }
    }""" shouldNot typeCheck

    """
    @multitier
    object test extends test {
      trait MyRegistry extends Registry
      placed[MyRegistry] { implicit! => val a = url.asLocal }
    }""" shouldNot typeCheck
  }

  it should "use correct types in a complex example" in {
    """
    @multitier
    class Test {
      trait Server extends Peer {
        type Tie <: Multiple[Client] with Single[SubClient1[_]] with Single[SubClient2]
      }

      trait Client extends Peer {
        type Tie <: Single[Server]
      }

      trait SubClient1[T] extends Client

      trait SubClient2 extends Client

      type ClientAlias = Client


      class Clazz

      implicit val marshallable: MarshallableArgument[(Int, String, Clazz)] = ???


      val global = 6


      placed[Server] { implicit! =>
        val client: Remote[Client] = ???
        val subClient1: Remote[SubClient1[_]] = ???
        val subClient2: Remote[SubClient2] = ???


        val a = (remote[SubClient2] call fun0(0, "")).asLocal
        val b = (remote call fun0(0, "")).asLocal
        val c = (remote.on(client) call fun0(0, "")).asLocal

        a.staticAssertType[Int]
        b.staticAssertType[List[Int]]
        c.staticAssertType[Int]


        val d = (remote.on(subClient1, subClient2) call fun0(0, "")).asLocal
        val e = (remote[Client].on(subClient1) call fun0(0, "")).asLocal
        val f = (remote.on(subClient1) call fun0(0, "")).asLocal
        val g = (remote[SubClient1[_]] call fun0(0, "")).asLocal

        d.staticAssertType[List[Int]]
        e.staticAssertType[Int]
        f.staticAssertType[Int]
        g.staticAssertType[Int]


        val h = vari.asLocal
        val i = (remote set vari := 10).asLocal
        val j = (remote[SubClient2] set vari := 10).asLocal
        val k = (remote.on(subClient2) set vari := 10).asLocal

        h.staticAssertType[List[Int]]
        i.staticAssertType[List[Unit]]
        j.staticAssertType[Unit]
        k.staticAssertType[Unit]


        val string = ""
        val clazz = new Clazz

        val l = serverVari + serverValue
        val m = remote[Client] { implicit! => 8 + 9 }.asLocal
        val n = remote[SubClient2].capture(l) { implicit! => l + 8 }.asLocal
        val o = remote.on(subClient1).capture(l, string, clazz) { implicit! => string + l }.asLocal
        val p = remote[Client].sbj { implicit! => 8 + 9 }.asLocal
        val q = remote[Client].sbj.capture(l) { implicit! => l + 8 }.asLocal
        val r = remote.on(subClient1).sbj.capture(l, serverValue) { implicit! => server: Remote[Server] => l + serverValue }.asLocal

        l.staticAssertType[Int]
        m.staticAssertType[List[Int]]
        n.staticAssertType[Int]
        o.staticAssertType[String]
        p.staticAssertType[List[Int]]
        q.staticAssertType[List[Int]]
        r.staticAssertType[Int]


        val s = subjectiveFun("")(client)
        val t = remote[Client] { implicit! => remote[Server] { implicit! => serverValue }.asLocal }.asLocal
        val u = remote[Client] { implicit! =>
          val serverValue = ""
          remote[Server].capture(serverValue) { implicit! =>
            val a = vari.asLocal
            val b = vari.from[Client].asLocal
            val c = vari.from[SubClient1[_]].asLocal
            val d = vari.from[SubClient2].asLocal
            val e = (remote call fun1(0.0)("", 0)).asLocal

            a.staticAssertType[List[Int]]
            b.staticAssertType[List[Int]]
            c.staticAssertType[Int]
            d.staticAssertType[Int]
            e.staticAssertType[List[Int]]

            serverValue
          }.asLocal
        }.asLocal

        s.staticAssertType[Int]
        t.staticAssertType[List[Int]]
        u.staticAssertType[List[String]]
      }


      placed[Client] { implicit! =>
        vari = 42
        val a = 42 + vari

        val b = subjectiveB.asLocal
        val c = subjectiveC.asLocal
        val d = subjectiveValue.asLocal
        val e = (remote call subjectiveFun("")).asLocal

        a.staticAssertType[Int]
        b.staticAssertType[Int]
        c.staticAssertType[Int]
        d.staticAssertType[Int]
        e.staticAssertType[Int]
      }


      lazy val serverValue = placed[Server] { implicit! => 5 + global }

      var serverVari = placed[Server] { implicit! => 5 + global }

      var vari = placed[Client] { implicit! => 5 + global }

      def fun0(d: Double, s: String): Int on Client = 5

      def fun1(d: Double)(s: String, i: Int): Int on Client = 5


      val subjectiveValue = placed[Server].sbj { implicit! => x: Remote[Client] => 8 }

      def subjectiveFun(s: String) = placed[Server].sbj { implicit! => x: Remote[Client] => 8 }


      val subjectiveA = placed[Server].local { implicit! => x: Remote[Client] => 8 }

      val subjectiveB = placed[Server].sbj { implicit! => x: Remote[Client] => 8 }

      val subjectiveC = placed[Server].sbj[Client] { implicit! => 8 }

      val subjectiveD: Remote[Client] <-> (Remote[Client] => Int) localOn Server = placed { implicit! => x: Remote[Client] => 8 }

      val assertSubjectiveA: (Remote[Client] => Int) localOn Server = subjectiveA

      val assertSubjectiveB: Remote[Client] <=> Int on Server = subjectiveB

      val assertSubjectiveC: Remote[Client] <-> Int on Server = subjectiveC


      val clientVal = placed[Client] { implicit! => 5 }

      val subClient1Val = placed[SubClient1[_]].overriding(clientVal) { implicit! =>
        (placed base clientVal) + 5
      }

      val subClient2Val = placed[SubClient2].overriding(clientVal) { implicit! =>
        (placed base clientVal) + 5
      }


      def clientDef(d: Double)(s: String, i: Int) = placed[Client] { implicit! => 5 }

      def subClient1Def(d: Double)(s: String, i: Int) = placed[SubClient1[_]].overriding(clientDef _) { implicit! =>
        (placed base clientDef(d)(s, i)) + 5
      }

      def subClient2Def(d: Double)(s: String, i: Int) = placed[SubClient2].overriding(clientDef _) { implicit! =>
        (placed base clientDef(d)(s, i)) + 5
      }
    }""" should compile
  }
}
