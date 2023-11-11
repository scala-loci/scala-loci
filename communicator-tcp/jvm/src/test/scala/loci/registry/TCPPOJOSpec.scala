package loci.registry

import loci.NoLogging
import loci.communicator.ConnectionSetupFactory
import loci.communicator.tcp.TCP

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TCPPOJOSpec extends AnyFlatSpec with Matchers with NoLogging {

  "TCPListener" should "be equal" in {
    TCP(1, "localhost") shouldBe TCP(1, "localhost")
  }

  "TCPConnector" should "be equal" in {
    TCP("localhost", 2) shouldBe TCP("localhost", 2)
  }

  "TCPListener" should "parse equal" in {
    val props: ConnectionSetupFactory.Properties = Map(
      "heartbeat-delay"   -> ("1h" :: Nil), // default: 3s
      "heartbeat-timeout" -> ("2h" :: Nil), // default: 10s
      "no-delay"          -> ("false" :: Nil) // default: true
    )
    props shouldBe props
    TCP.listener("tcp://localhost:3", props) shouldBe TCP.listener("tcp://localhost:3", props)
  }

  "TCPConnector" should "parse equal" in {
    val props: ConnectionSetupFactory.Properties = Map(
      "heartbeat-delay"   -> ("3h" :: Nil), // default: 3s
      "heartbeat-timeout" -> ("4h" :: Nil), // default: 10s
      "no-delay"          -> ("false" :: Nil) // default: true
    )
    props shouldBe props
    TCP.connector("tcp://localhost:4", props) shouldBe TCP.connector("tcp://localhost:4", props)
  }

}
