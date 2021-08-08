package loci
package examples

import communicator.NetworkListener
import transmitter.Serializables._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable


@multitier trait GSet[T] {
  @peer type Node <: { type Tie <: Multiple[Node] }

  private val content = on[Node] local { mutable.Set.empty[T] }

  def contains(v: T) = on[Node] { implicit! =>
    content contains v
  }

  def add(v: T) = on[Node] { implicit! =>
    content += v
    remote call merge(content.toSet)
  }

  private def merge(content: Set[T]): Unit on Node =
    this.content ++= content
}

@multitier object distributedSetApp {
  @peer type Server <: ints.Node with strings.Node { type Tie <: Single[Client] with Single[ints.Node] with Single[strings.Node] }
  @peer type Client <: ints.Node with strings.Node { type Tie <: Single[Server] with Single[ints.Node] with Single[strings.Node] }

  @multitier object ints extends GSet[Int]
  @multitier object strings extends GSet[String]
}


class DistributedSetSpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "Distributed set example"

  it should "run correctly" in {
    val listener = new NetworkListener

    val serverInstance = multitier start new Instance[distributedSetApp.Server](
      contexts.Immediate.global,
      listen[distributedSetApp.Client] { listener })

    val clientInstance = multitier start new Instance[distributedSetApp.Client](
      contexts.Immediate.global,
      connect[distributedSetApp.Server] { listener.createConnector() })


    serverInstance.instance.current map { _ retrieve (distributedSetApp.ints contains 42) } should be (Some(false))
    clientInstance.instance.current map { _ retrieve (distributedSetApp.ints contains 42) } should be (Some(false))

    serverInstance.instance.current map { _ retrieve (distributedSetApp.ints contains 84) } should be (Some(false))
    clientInstance.instance.current map { _ retrieve (distributedSetApp.ints contains 84) } should be (Some(false))

    serverInstance.instance.current map { _ retrieve (distributedSetApp.ints contains 21) } should be (Some(false))
    clientInstance.instance.current map { _ retrieve (distributedSetApp.ints contains 21) } should be (Some(false))

    serverInstance.instance.current map { _ retrieve (distributedSetApp.strings contains "fourty-two") } should be (Some(false))
    clientInstance.instance.current map { _ retrieve (distributedSetApp.strings contains "fourty-two") } should be (Some(false))

    serverInstance.instance.current map { _ retrieve (distributedSetApp.strings contains "eighty-four") } should be (Some(false))
    clientInstance.instance.current map { _ retrieve (distributedSetApp.strings contains "eighty-four") } should be (Some(false))

    serverInstance.instance.current map { _ retrieve (distributedSetApp.strings contains "twenty-one") } should be (Some(false))
    clientInstance.instance.current map { _ retrieve (distributedSetApp.strings contains "twenty-one") } should be (Some(false))


    serverInstance.instance.current foreach { _ retrieve (distributedSetApp.ints add 42) }


    serverInstance.instance.current map { _ retrieve (distributedSetApp.ints contains 42) } should be (Some(true))
    clientInstance.instance.current map { _ retrieve (distributedSetApp.ints contains 42) } should be (Some(true))

    serverInstance.instance.current map { _ retrieve (distributedSetApp.ints contains 84) } should be (Some(false))
    clientInstance.instance.current map { _ retrieve (distributedSetApp.ints contains 84) } should be (Some(false))

    serverInstance.instance.current map { _ retrieve (distributedSetApp.ints contains 21) } should be (Some(false))
    clientInstance.instance.current map { _ retrieve (distributedSetApp.ints contains 21) } should be (Some(false))

    serverInstance.instance.current map { _ retrieve (distributedSetApp.strings contains "fourty-two") } should be (Some(false))
    clientInstance.instance.current map { _ retrieve (distributedSetApp.strings contains "fourty-two") } should be (Some(false))

    serverInstance.instance.current map { _ retrieve (distributedSetApp.strings contains "eighty-four") } should be (Some(false))
    clientInstance.instance.current map { _ retrieve (distributedSetApp.strings contains "eighty-four") } should be (Some(false))

    serverInstance.instance.current map { _ retrieve (distributedSetApp.strings contains "twenty-one") } should be (Some(false))
    clientInstance.instance.current map { _ retrieve (distributedSetApp.strings contains "twenty-one") } should be (Some(false))


    clientInstance.instance.current foreach { _ retrieve (distributedSetApp.ints add 84) }


    serverInstance.instance.current map { _ retrieve (distributedSetApp.ints contains 42) } should be (Some(true))
    clientInstance.instance.current map { _ retrieve (distributedSetApp.ints contains 42) } should be (Some(true))

    serverInstance.instance.current map { _ retrieve (distributedSetApp.ints contains 84) } should be (Some(true))
    clientInstance.instance.current map { _ retrieve (distributedSetApp.ints contains 84) } should be (Some(true))

    serverInstance.instance.current map { _ retrieve (distributedSetApp.ints contains 21) } should be (Some(false))
    clientInstance.instance.current map { _ retrieve (distributedSetApp.ints contains 21) } should be (Some(false))

    serverInstance.instance.current map { _ retrieve (distributedSetApp.strings contains "fourty-two") } should be (Some(false))
    clientInstance.instance.current map { _ retrieve (distributedSetApp.strings contains "fourty-two") } should be (Some(false))

    serverInstance.instance.current map { _ retrieve (distributedSetApp.strings contains "eighty-four") } should be (Some(false))
    clientInstance.instance.current map { _ retrieve (distributedSetApp.strings contains "eighty-four") } should be (Some(false))

    serverInstance.instance.current map { _ retrieve (distributedSetApp.strings contains "twenty-one") } should be (Some(false))
    clientInstance.instance.current map { _ retrieve (distributedSetApp.strings contains "twenty-one") } should be (Some(false))


    clientInstance.instance.current foreach { _ retrieve (distributedSetApp.strings add "fourty-two") }


    serverInstance.instance.current map { _ retrieve (distributedSetApp.ints contains 42) } should be (Some(true))
    clientInstance.instance.current map { _ retrieve (distributedSetApp.ints contains 42) } should be (Some(true))

    serverInstance.instance.current map { _ retrieve (distributedSetApp.ints contains 84) } should be (Some(true))
    clientInstance.instance.current map { _ retrieve (distributedSetApp.ints contains 84) } should be (Some(true))

    serverInstance.instance.current map { _ retrieve (distributedSetApp.ints contains 21) } should be (Some(false))
    clientInstance.instance.current map { _ retrieve (distributedSetApp.ints contains 21) } should be (Some(false))

    serverInstance.instance.current map { _ retrieve (distributedSetApp.strings contains "fourty-two") } should be (Some(true))
    clientInstance.instance.current map { _ retrieve (distributedSetApp.strings contains "fourty-two") } should be (Some(true))

    serverInstance.instance.current map { _ retrieve (distributedSetApp.strings contains "eighty-four") } should be (Some(false))
    clientInstance.instance.current map { _ retrieve (distributedSetApp.strings contains "eighty-four") } should be (Some(false))

    serverInstance.instance.current map { _ retrieve (distributedSetApp.strings contains "twenty-one") } should be (Some(false))
    clientInstance.instance.current map { _ retrieve (distributedSetApp.strings contains "twenty-one") } should be (Some(false))


    serverInstance.instance.current foreach { _ retrieve (distributedSetApp.strings add "eighty-four") }


    serverInstance.instance.current map { _ retrieve (distributedSetApp.ints contains 42) } should be (Some(true))
    clientInstance.instance.current map { _ retrieve (distributedSetApp.ints contains 42) } should be (Some(true))

    serverInstance.instance.current map { _ retrieve (distributedSetApp.ints contains 84) } should be (Some(true))
    clientInstance.instance.current map { _ retrieve (distributedSetApp.ints contains 84) } should be (Some(true))

    serverInstance.instance.current map { _ retrieve (distributedSetApp.ints contains 21) } should be (Some(false))
    clientInstance.instance.current map { _ retrieve (distributedSetApp.ints contains 21) } should be (Some(false))

    serverInstance.instance.current map { _ retrieve (distributedSetApp.strings contains "fourty-two") } should be (Some(true))
    clientInstance.instance.current map { _ retrieve (distributedSetApp.strings contains "fourty-two") } should be (Some(true))

    serverInstance.instance.current map { _ retrieve (distributedSetApp.strings contains "eighty-four") } should be (Some(true))
    clientInstance.instance.current map { _ retrieve (distributedSetApp.strings contains "eighty-four") } should be (Some(true))

    serverInstance.instance.current map { _ retrieve (distributedSetApp.strings contains "twenty-one") } should be (Some(false))
    clientInstance.instance.current map { _ retrieve (distributedSetApp.strings contains "twenty-one") } should be (Some(false))


    clientInstance.terminate()
    serverInstance.terminate()
  }
}
