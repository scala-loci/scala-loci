package loci
package communicator

import scala.concurrent.ExecutionContext
import scala.util.{Success, Try}

sealed trait ConnectionSetup[+P <: ProtocolCommon]

trait Connector[+P <: ProtocolCommon] extends ConnectionSetup[P] {
  final def connect(failureReporter: Throwable => Unit = ExecutionContext.defaultReporter)
      (handler: Try[Connection[P]] => Unit): Unit = {
    val connected = Notice.Steady[Try[Connection[P]]]
    connected.notice foreach handler
    connect(connected)
  }

  protected type Connected[-C <: ProtocolCommon] = Notice.Steady.Source[Try[Connection[C]]]

  protected def connect(connectionEstablished: Connected[P]): Unit
}

trait Listener[+P <: ProtocolCommon] extends ConnectionSetup[P] {
  final def startListening(failureReporter: Throwable => Unit = ExecutionContext.defaultReporter)
      (handler: Try[Connection[P]] => Unit): Try[Listening] = {
    val connected = Notice.Stream[Try[Connection[P]]]
    connected.notice foreach handler
    startListening(connected)
  }

  protected type Connected[-C <: ProtocolCommon] = Notice.Stream.Source[Try[Connection[C]]]

  protected def startListening(connectionEstablished: Connected[P]): Try[Listening]

  final def firstConnection: Connector[P] = new Connector[P] {
    protected def connect(connectionEstablished: Connected[P]): Unit = {
      var firstConnection: Connection[P] = null
      var listening: Try[Listening] = null

      listening = startListening() {
        case success @ Success(connection) =>
          if (connectionEstablished.trySet(success)) {
            connection.closed foreach { _ =>
              if (listening != null)
                listening foreach { _.stopListening() }
            }
            if (listening != null && !connection.open)
              listening foreach { _.stopListening() }
            firstConnection = connection
          }
          else
            connection.close()

        case failure =>
          connectionEstablished.trySet(failure)
      }

      if (firstConnection != null && !firstConnection.open)
        listening foreach { _.stopListening() }
    }
  }
}

trait Listening {
  def stopListening(): Unit
}
