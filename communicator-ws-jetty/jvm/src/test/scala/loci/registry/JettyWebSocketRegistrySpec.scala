package loci
package registry

import communicator.ws.jetty._

import org.eclipse.jetty.server.handler.ContextHandler
import org.eclipse.jetty.server.{Server, ServerConnector}
import org.eclipse.jetty.websocket.server.WebSocketUpgradeHandler

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

import scala.util.{Success, Try}

class JettyWebSocketRegistrySpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "Jetty WebSocket Registry"

  val port = 45852

  def run(test: RegistryTests.Test) = {
    var server: Server = null

    def setup = {
      if (server != null)
        server.stop()

      server = new Server()

      val connector = new ServerConnector(server)
      connector.setPort(port)
      server.addConnector(connector)

      val context = new ContextHandler()
      server.setHandler(context)

      val webSocketHandler = WebSocketUpgradeHandler.from(server, context)
      context.setHandler(webSocketHandler)

      Try {
        server.start()
        webSocketHandler
      }
    }

    test(
      setup map { WS(_, "/registry/*") },
      Success(WS(s"ws://localhost:$port/registry/")),
      Try { server.stop() })
  }

  it should "handle binding and lookup correctly" in {
    for (_ <- 1 to 100)
      run(RegistryTests.`handle binding and lookup correctly`)
  }

  it should "handle subjective binding and lookup correctly" in {
    for (_ <- 1 to 100)
      run(RegistryTests.`handle subjective binding and lookup correctly`)
  }
}
