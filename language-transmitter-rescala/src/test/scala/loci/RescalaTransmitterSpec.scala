package loci

import language._
import language.transmitter.rescala._
import communicator.NetworkListener
import serializer.Serializables._

import rescala.default._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

@multitier final class RescalaTransmitterApplication(
    signalValues: => mutable.ListBuffer[Int],
    eventValues: => mutable.ListBuffer[Int]) {
  @peer type Server <: { type Tie <: Multiple[Client] }
  @peer type Client <: { type Tie <: Single[Server] }

  val event: Evt[Int] on Client = Evt[Int]()
  val signal = on[Server] { Var(42) }

  on[Client] { signal.asLocal observe { signalValues += _ } }

  on[Server] { event.asLocalFromAllSeq observe { case (_, value) => eventValues += value } }
}

class RescalaTransmitterSpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "REScala transmitter"

  it should "handle binding and lookup for values and functions correctly" in {
    val signalValues = mutable.ListBuffer.empty[Int]
    val eventValues = mutable.ListBuffer.empty[Int]

    val listener = new NetworkListener

    val application = new RescalaTransmitterApplication(signalValues, eventValues)

    val serverInstance = multitier start new Instance[application.Server](
      contexts.Immediate.global,
      listen[application.Client] { listener })

    val clientInstance = multitier start new Instance[application.Client](
      contexts.Immediate.global,
      connect[application.Server] { listener.createConnector() })

    signalValues should contain theSameElementsAs Seq(42)
    eventValues should contain theSameElementsAs Seq()

    clientInstance.instance foreach { instance =>
      (instance retrieve application.event).fire(12)
    }

    signalValues should contain theSameElementsAs Seq(42)
    eventValues should contain theSameElementsAs Seq(12)

    clientInstance.terminate()
    serverInstance.terminate()
  }
}
