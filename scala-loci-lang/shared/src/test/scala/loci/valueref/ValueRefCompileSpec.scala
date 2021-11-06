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

      def nonAccess(ref: Int via A): Int via A on B = on[B] { implicit! =>
        println(ref)
        val ref2: Int via A = ref.copy()
        ref2
      }
    }""" should compile
  }

  it should "not compile accessed value references on an untied peer" in {
    """@multitier object ValueRefTransmissionModule {
      @peer type A
      @peer type B

      def access(ref: Int via A): Future[Int] on B = on[B] { implicit! =>
        ref.getValue
      }
    }""" shouldNot compile
  }

}
