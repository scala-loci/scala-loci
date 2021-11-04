package loci
package registry

import communicator.ws.akka._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class AkkaWebSocketRegistrySpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "Akka WebSocket Registry"

  val port = 45848

  it should "handle binding and lookup correctly" in {
    RegistryTests.`handle binding and lookup correctly`(WS(port), WS(s"ws://localhost:$port"))
  }

  it should "handle subjective binding and lookup correctly" in {
    RegistryTests.`handle subjective binding and lookup correctly`(WS(port), WS(s"ws://localhost:$port"))
  }
}
