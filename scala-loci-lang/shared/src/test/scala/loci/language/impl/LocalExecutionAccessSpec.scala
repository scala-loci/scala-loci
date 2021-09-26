package loci
package language
package impl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import transmitter.Serializables._
import scala.concurrent.Future

class LocalExecutionAccessSpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "Accessing the value of a local execution"

  it should "compile for a SelfReferenceDummyRequest on a Future value" in {
    """@multitier object Module {
      @peer type Node <: { type Tie <: Multiple[Node] }

      def select(connected: Seq[Remote[Node]], self: SelfReference[Node]): Local[Remote[Node]] on Node = on[Node] { implicit! =>
        self
      }

      def f(): Future[Int] on Node = on[Node] { Future.successful(42) }

      def run(): Unit on Node = on[Node] { implicit! =>
        val x: Future[Int] = remoteAny.apply[Node](select _).call(f()).asLocal
      }
    }""" should compile
  }

  it should "compile for a SelfReferenceDummyRequest on a non-Future value" in {
    """@multitier object Module {
      @peer type Node <: { type Tie <: Multiple[Node] }

      def select(connected: Seq[Remote[Node]], self: SelfReference[Node]): Local[Remote[Node]] on Node = on[Node] { implicit! =>
        self
      }

      def f(): Int on Node = on[Node] { 42 }

      def run(): Unit on Node = on[Node] { implicit! =>
        val x: Future[Int] = remoteAny.apply[Node](select _).call(f()).asLocal
      }
    }""" should compile
  }

}
