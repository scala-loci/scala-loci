package loci
package impl

import communicator._

import scala.util.Random
import scala.util.Success
import scala.collection.mutable.ListBuffer

import DirectConnectionSimulation.SimulationProtocol

object DirectConnectionSimulation {
  type SimulationProtocol = ProtocolCommon with Bidirectional
}

trait DirectConnectionSimulation {
  protected val random: Random

  protected var deferred: Boolean

  protected var locked = false

  protected val events = ListBuffer.empty[(DirectConnection, () => Unit)]

  protected def evaluateEvents() =
    if (!deferred && !locked) {
      locked = true
      while (events.nonEmpty) {
        val (connection, _) = events(random nextInt events.size)
        val index = events indexWhere {
          case (elementConnection, _) => elementConnection == connection
        }

        val (_, event) = events remove index
        event()
      }
      locked = false
    }

  def run() = {
    deferred = false
    evaluateEvents
  }

  class DirectConnection(connectionSetup: ConnectionSetup[SimulationProtocol])
      extends Connection[SimulationProtocol] {
    var connection: DirectConnection = _
    var isOpen = true
    val doClosed = Notifier[Unit]
    val doReceive = Notifier[MessageBuffer]

    val protocol = new Protocol
        with SetupInfo with SecurityInfo with SymmetryInfo with Bidirectional {
      val setup = connectionSetup
      val encrypted = false
      val integrityProtected = false
      val authenticated = false
    }
    val closed = doClosed.notification
    val receive = doReceive.notification

    def open = isOpen

    def close() = {
      isOpen = false
      doClosed()
      events += this -> { () =>
        connection.isOpen = false
        connection.doClosed()
      }
      evaluateEvents
    }

    def send(data: MessageBuffer) = {
      events += this -> { () => connection.doReceive(data) }
      evaluateEvents
    }
  }
}

class NetworkListener(
    protected var deferred: Boolean = false,
    protected val seed: Int = 0)
  extends DirectConnectionSimulation with Listener[SimulationProtocol] {

  protected val random = new Random(seed.toLong)

  protected val handlers = ListBuffer.empty[Handler[SimulationProtocol]]

  def createConnector = new Connector[SimulationProtocol] {
    protected def connect(handler: Handler[SimulationProtocol]) = {
      val connection0 = new DirectConnection(NetworkListener.this)
      val connection1 = new DirectConnection(this)
      connection0.connection = connection1
      connection1.connection = connection0
      handlers foreach { _ notify Success(connection0) }
      handler notify Success(connection1)
    }
  }

  protected def startListening(handler: Handler[SimulationProtocol]) = {
    handlers += handler
    Success(new Listening {
      def stopListening() = { }
    })
  }
}

class NetworkConnector(
    protected var deferred: Boolean = false,
    protected val seed: Int = 0)
  extends DirectConnectionSimulation {

  protected val random = new Random(seed.toLong)

  val first = new Connector[SimulationProtocol] {
    protected def connect(handler: Handler[SimulationProtocol]) =
      handler notify Success(connection0)
  }

  val second = new Connector[SimulationProtocol] {
    protected def connect(handler: Handler[SimulationProtocol]) =
      handler notify Success(connection1)
  }

  protected val connection0: DirectConnection = new DirectConnection(first)
  protected val connection1: DirectConnection = new DirectConnection(second)
  connection0.connection = connection1
  connection1.connection = connection0
}
