package loci
package registry

import contexts.Immediate.Implicits.global
import loci.communicator.ws.jetty._
import loci.serializer.upickle._
import loci.transmitter.RemoteAccessException
import org.eclipse.jetty.server.{Server, ServerConnector}
import org.eclipse.jetty.servlet.ServletContextHandler
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable
import scala.concurrent.{Future, Promise}

class RegistrySpec extends AnyFlatSpec with Matchers {
  logging.root.clearHandlers().clearModifiers().replace()

  behavior of "Registry"

  it should "handle binding and lookup correctly" in {
    for (seed <- 0 to 10) {

      val server    = new Server()
      val connector = new ServerConnector(server)
      server.addConnector(connector)

      val context = new ServletContextHandler(ServletContextHandler.SESSIONS)
      server.setHandler(context)

      val listener = WS(context, "/registry/*")

      val promise = Promise[(Int, String)]()

      val registry0 = new Registry
      registry0.bind("future")(promise.future)
      registry0.bind("intfun")(() => (???): Int)
      registry0.listen(listener)

      connector.setPort(8080)
      server.start()

      registry0.remotes.foreach(println)

      var futureValue: Future[(Int, String)] = null
      var intfunValue: Future[Int]           = null

      if (seed % 2 == 0)
        promise.success(5 -> "yay")

      val registry1 = new Registry
      registry1.connect(WS("ws://localhost:8080/registry/")) foreach { remote =>
        val result0 = registry1.lookup[concurrent.Future[(Int, String)]]("future", remote)
        val result1 = registry1.lookup[() => Int]("intfun", remote)
        futureValue = result0
        intfunValue = result1()
      }

      if (seed % 2 != 0)
        promise.success(5 -> "yay")

      Thread.sleep(100)

      assert(futureValue.value.get.get == (5 -> "yay"))

      registry0.terminate()
      registry1.terminate()

      server.stop()

      val remoteException = intercept[RemoteAccessException] {intfunValue.value.get.get}
      remoteException.reason should matchPattern { case RemoteAccessException.RemoteException("scala.NotImplementedError", _) => }
    }
  }

  it should "handle subjective binding and lookup correctly" in {
    for (seed <- 0 to 5) {
      val events = mutable.ListBuffer.empty[String]

      val server    = new Server()
      val connector = new ServerConnector(server)
      server.addConnector(connector)

      val context = new ServletContextHandler(ServletContextHandler.SESSIONS)
      server.setHandler(context)

      val listener = WS(context, "/registry/*")

      val valueBinding  = registry.Binding[String]("value")
      val methodBinding = registry.Binding[() => String]("method")

      def value(remote: transmitter.RemoteRef) = {
        events += "value called"
        "value result"
      }

      def method(remote: transmitter.RemoteRef) = {
        events += "method called"
        "method result"
      }

      val registry0 = new Registry
      registry0.bindSbj(valueBinding)(value _)
      registry0.bindSbj(methodBinding)(method _)
      registry0.listen(listener)

      connector.setPort(8080)
      server.start()

      val registry1 = new Registry
      registry1.connect(WS("ws://localhost:8080/registry/")) foreach { remote =>
        val result0 = registry1.lookup(valueBinding, remote)
        val result1 = registry1.lookup(methodBinding, remote)

        result0 foreach {events += _}
        result0 foreach {events += _}
        result1() foreach {events += _}
        result1() foreach {events += _}
      }

      Thread.sleep(100)

      registry0.terminate()
      registry1.terminate()

      server.stop()

      events should contain theSameElementsAs Seq(
        "value called",
        "value result",
        "value result",
        "method called",
        "method result",
        "method called",
        "method result")

      events filter {_ startsWith "value"} should contain theSameElementsInOrderAs Seq(
        "value called", "value result", "value result")

      events find {_ startsWith "method"} should contain("method called")

      events.remove(events.indexOf("method called"))
      events.remove(events.indexOf("method result"))

      events filterNot {_ startsWith "value"} should contain theSameElementsInOrderAs Seq(
        "method called", "method result")
    }
  }
}
