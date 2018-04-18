package retier
package impl

import util._
import network._

import scala.util.Random
import scala.concurrent.Future
import scala.collection.mutable.ListBuffer

trait DirectConnectionSimulation {
  protected object protocolInfo extends ProtocolInfo {
    def establisher = ???
    def isEncrypted = false
    def isProtected = false
    def isAuthenticated = false
    def identification = None
  }

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

  class DirectConnection extends Connection {
    var connection: DirectConnection = _
    var open = true
    val doClosed = Notifier[Unit]
    val doReceive = Notifier[String]

    val protocol = protocolInfo
    val closed = doClosed.notification
    val receive = doReceive.notification

    def isOpen = open

    def close() = {
      open = false
      doClosed()
      events += this -> { () =>
        connection.open = false
        connection.doClosed()
      }
      evaluateEvents
    }

    def send(data: String) = {
      events += this -> { () => connection.doReceive(data) }
      evaluateEvents
    }
  }
}

class NetworkListener(
    protected var deferred: Boolean = false,
    protected val seed: Int = 0)
  extends DirectConnectionSimulation with ConnectionListener {

  protected val random = new Random(seed.toLong)

  def createRequestor: ConnectionRequestor = new ConnectionRequestor {
    def request = {
      val connection0 = new DirectConnection
      val connection1 = new DirectConnection
      connection0.connection = connection1
      connection1.connection = connection0
      doConnectionEstablished(connection0)
      Future successful connection1
    }
  }

  def start() = { }
  def stop() = { }
}

class NetworkRequestor(
    protected var deferred: Boolean = false,
    protected val seed: Int = 0)
  extends DirectConnectionSimulation {

  protected val random = new Random(seed.toLong)

  protected val connection0 = new DirectConnection
  protected val connection1 = new DirectConnection
  connection0.connection = connection1
  connection1.connection = connection0

  val first = new ConnectionRequestor {
    def request = Future successful connection0
  }

  val second = new ConnectionRequestor {
    def request = Future successful connection1
  }
}
