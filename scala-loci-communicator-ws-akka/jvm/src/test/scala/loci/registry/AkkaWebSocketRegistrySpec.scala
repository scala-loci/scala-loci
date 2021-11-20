package loci
package registry

import communicator.ws.akka._

import java.io.{OutputStream, PrintStream}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

import scala.concurrent.duration._

class AkkaWebSocketRegistrySpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "Akka WebSocket Registry"

  val port = 45848

  val silentStream = new PrintStream(new OutputStream {
    def write(b: Int) = { }
  })

  it should "handle binding and lookup correctly" in {
    for (_ <- 1 to 50) {
      val listener = WS(port)
      val connector = WS(s"ws://localhost:$port")

      Console.withOut(silentStream) {
        RegistryTests.`handle binding and lookup correctly`(
          listener,
          connector,
          setupListener = awaitBinding(listener, 1.minute),
          cleanup = awaitTermination(1.minute))
      }
    }
  }

  it should "handle subjective binding and lookup correctly" in {
    for (_ <- 1 to 50) {
      val listener = WS(port)
      val connector = WS(s"ws://localhost:$port")

      Console.withOut(silentStream) {
        RegistryTests.`handle subjective binding and lookup correctly`(
          listener,
          connector,
          setupListener = awaitBinding(listener, 1.minute),
          cleanup = awaitTermination(1.minute))
      }
    }
  }
}
