package loci
package language
package impl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PeergroupSpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "Peergroups"

  it should "compile for a missing implementation on a peergroup with all concrete peers implemented" in {
    """@multitier object AbstractPeer {
      @peergroup type Node
      @peer type A <: Node { type Tie <: Multiple[Node] }
      @peer type B <: Node { type Tie <: Multiple[Node] }

      def f(x: Int): Int on Node = on[A] { implicit! =>
        x + 1
      } and on[B] { implicit! =>
        x + 2
      }
    }""" should compile
  }

  it should "compile for a missing implementation on the top-level peer group with a lower-level peergroup being implemented" in {
    """@multitier object AbstractPeer {
      @peergroup type Node
      @peergroup type AB <: Node
      @peer type A <: AB { type Tie <: Multiple[Node] }
      @peer type B <: AB { type Tie <: Multiple[Node] }
      @peer type C <: Node { type Tie <: Multiple[Node] }

      def f(x: Int): Int on Node = on[AB] { implicit! =>
        x + 1
      } and on[C] { implicit! =>
        x + 2
      }
    }""" should compile
  }

  it should "not compile for a missing concrete peer implementation of a non-leaf peer" in {
    """@multitier object AbstractPeer {
      @peer type Node
      @peer type A <: Node { type Tie <: Multiple[Node] }
      @peer type B <: Node { type Tie <: Multiple[Node] }

      def f(x: Int): Int on Node = on[A] { implicit! =>
        x + 1
      } and on[B] { implicit! =>
        x + 2
      }
    }""" shouldNot compile
  }

  it should "not compile for a missing concrete peer implementation of a leaf peer" in {
    """@multitier object AbstractPeer {
      @peergroup type Node
      @peer type A <: Node { type Tie <: Multiple[Node] }
      @peer type B <: Node { type Tie <: Multiple[Node] }

      def f(x: Int): Int on Node = on[A] { implicit! =>
        x + 1
      }
    }""" shouldNot compile
  }

  it should "not compile for an implementation on a peer that is not a sub-peer of the top-level peer" in {
    """@multitier object AbstractPeer {
      @peergroup type Node
      @peer type A <: Node { type Tie <: Multiple[Node] }
      @peer type B <: Node { type Tie <: Multiple[Node] }
      @peer type C <: { type Tie <: Multiple[Node] }

      def f(x: Int): Int on Node = on[A] { implicit! =>
        x + 1
      } and on[B] { implicit! =>
        x + 2
      } and on[C] { implicit! =>
        x + 3
      }
    }""" shouldNot compile
  }
}
