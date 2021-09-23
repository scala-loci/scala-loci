package loci
package language
package impl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RecursiveRemoteCallExpansionSpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "Recursive remote call expansion"

  it should "compile for recursive placement of a remote call with multiple parameter lists" in {
    """@multitier object Module {
      @peer type Node <: { type Tie <: Multiple[Node] }
      @peer type A <: Node { type Tie <: Multiple[Node] }
      @peer type B <: Node { type Tie <: Multiple[Node] }
      @peer type C <: Node { type Tie <: Multiple[Node] }

      def select(connected: Seq[Remote[Node]], self: SelfReference[Node]): Local[Remote[Node]] on Node = on[Node] { implicit! =>
        throw new NotImplementedError
      } and on[A] { implicit! =>
        connected.flatMap(_.asRemote[B]).head
      } and on[B] { implicit! =>
        connected.flatMap(_.asRemote[C]).head
      } and on[C] { implicit! =>
        self
      }

      def f(a: Int)(b: Int, c: String)(d: Double): Int on Node = on[Node] { implicit! =>
        throw new NotImplementedError
      } and on[A] { implicit! =>
        a + b
      } and on[B] { implicit! =>
        println(c)
        b
      } and on[C] { implicit! =>
        d.toInt
      }

      def remoteExecutions(): Unit on Node = on[Node] { implicit! =>
        val b = 42
        onAny.recursive[Node](select _).run.capture(b) { implicit! => f(1)(b, "test")(0.1) }
        remoteAny.recursive[Node](select _).call(f(1)(b, "test")(0.1))
        onAny.recursive[Node](select _).run.capture(b) { implicit! => f(1)(b, "test")(0.1) }.asLocal
        remoteAny.recursive[Node](select _).call(f(1)(b, "test")(0.1)).asLocal
        onAny.recursive[Node](select _).run.capture(b) { implicit! => f(1)(b, "test")(0.1) }.asLocal_!
        remoteAny.recursive[Node](select _).call(f(1)(b, "test")(0.1)).asLocal_!
      }
    }""" should compile
  }

}
