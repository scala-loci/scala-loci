package loci
package valueref

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ValueRefCompileSpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "Compiling usages of ValueRef"

  it should "compile non-accessed value references on an untied peer" in {
    """@multitier object ValueRefTransmissionModule {
      @peer type A
      @peer type B

      def nonAccess(ref: Int at A): Int at A on B = on[B] { implicit! =>
        println(ref)
        val ref2: Int at A = ref.copy()
        ref2
      }
    }""" should compile
  }

  it should "not compile accessed value references on an untied peer" in {
    """@multitier object ValueRefTransmissionModule {
      @peer type A
      @peer type B

      def access(ref: Int at A): Future[Int] on B = on[B] { implicit! =>
        ref.deref
      }
    }""" shouldNot compile
  }

  it should "compile access to the remote of a ValueRef" in {
    """@multitier object Module {
      @peer type Node <: { type Tie <: Multiple[Node] }

      def f(x: Int at Node): Unit on Node = on[Node] { implicit! =>
        val y: Remote[Node] = x.peer
      }
    }"""
  }

  it should "compile local access of a ValueRef" in {
    """@multitier object Module {
      @peer type Node

      def f(x: Int at Node): Unit on Node = on[Node] { implicit! =>
        val y: Int = x.derefLocally
      }
    }"""
  }

}
