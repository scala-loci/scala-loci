package loci
package registry

import communicator.ws.akka._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

import scala.concurrent.duration._
import scala.util.Success

class AkkaWebSocketRegistrySpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "Akka WebSocket Registry"

  val port = 45851

  it should "handle binding and lookup correctly" in {
    for (_ <- 1 to 100) {
      val listener = WS(port)
      val connector = WS(s"ws://localhost:$port")

      def setup = {
        awaitBinding(listener, 1.minute)
        Success(listener)
      }

      RegistryTests.`handle binding and lookup correctly`(
        setup,
        Success(connector),
        awaitTermination(1.minute))
    }
  }

  it should "handle subjective binding and lookup correctly" in {
    for (_ <- 1 to 100) {
      val listener = WS(port)
      val connector = WS(s"ws://localhost:$port")

      def setup = {
        awaitBinding(listener, 1.minute)
        Success(listener)
      }

      RegistryTests.`handle subjective binding and lookup correctly`(
        setup,
        Success(connector),
        awaitTermination(1.minute))
    }
  }
}
