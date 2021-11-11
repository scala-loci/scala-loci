package loci
package language
package impl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.concurrent.Future

class UnionPeerTypeSpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "Union peer types"

  it should "compile for a simple union of two peers" in {
    """@multitier object Module {
      @peer type A <: { type Tie <: Multiple[B] }
      @peer type B

      def f(x: Int): Int on (A | B) = on[A] { implicit! =>
        x + 1
      } and on[B] { implicit! =>
        x + 2
      }
    }""" should compile
  }

  it should "compile for a complex union of multiple peers and peergroups" in {
    """@multitier object Module {
      @peer type A <: { type Tie <: Multiple[B] }
      @peer type B
      @peergroup type T
      @peer type C <: T
      @peer type D <: T { type Tie <: Single[T] }
      @peergroup type U
      @peer type E <: U
      @peer type F <: U


      def f(x: Int): Int on (A | B | T | U) = on[A | T] { implicit! =>
        x + 1
      } and on[B | U] { implicit! =>
        x + 2
      }
    }""" should compile
  }

  it should "compile for a tie to a union peer type" in {
    """@multitier object Module {
      @peer type A <: { type Tie <: Single[A | B] }
      @peer type B

      def f(): Int on (A | B) = 5

      val y: Future[Int] on A = on[A] { implicit! =>
        remote[A | B].call(f()).asLocal
      }
    }""" should compile
  }

  it should "not compile for a missing implementation of a peer of the union" in {
    """@multitier object Module {
      @peer type A <: { type Tie <: Multiple[B] }
      @peer type B
      @peer type C

      def f(x: Int): Int on (A | B | C) = on[A | B] { implicit! =>
        x + 1
      }
    }""" shouldNot compile
  }

  it should "not compile for an implementation of a peer that is not part of the union" in {
    """@multitier object Module {
      @peer type A <: { type Tie <: Multiple[B] }
      @peer type B
      @peer type C

      def f(x: Int): Int on (A | B) = on[A] { implicit! =>
        x + 1
      } and on[B] { implicit! =>
        x + 2
      } and on[C] { implicit! =>
        x + 2
      }
    }""" shouldNot compile
  }

  it should "compile for a remote block on a concrete peer accessing a value placed on the union" in {
    """@multitier object Module {
      @peer type Node <: { type Tie <: Single[A] with Single[B] }
      @peer type A <: { type Tie <: Single[Node] }
      @peer type B <: { type Tie <: Single[Node] }

      val x: Int on (A | B) = on[A | B] { implicit! => 42 }

      val f: Future[Int] on Node = on[Node] { implicit! =>
        on[A].run { implicit! => x + 1 }.asLocal
      }
    }""" should compile
  }

}
