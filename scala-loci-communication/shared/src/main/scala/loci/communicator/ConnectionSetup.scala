package loci
package communicator

import scala.util.Try
import scala.util.Success
import scala.util.control.NonFatal
import scala.concurrent.ExecutionContext.defaultReporter
import java.util.concurrent.atomic.AtomicBoolean

sealed trait ConnectionSetup[+P <: ProtocolCommon] {
  protected trait Handler[-T <: ProtocolCommon] {
    def notify(connection: Try[Connection[T]]): Unit
  }
}

trait Connector[+P <: ProtocolCommon] extends ConnectionSetup[P] {
  final def connect(failureReporter: Throwable => Unit = defaultReporter)
      (handler: Try[Connection[P]] => Unit): Unit = {
    val applyHandler = new AtomicBoolean(true)
    connect(new Handler[P] {
      def notify(connection: Try[Connection[P]]) =
        if (applyHandler getAndSet false)
          try handler(connection)
          catch { case NonFatal(exception) => failureReporter(exception) }
    })
  }

  protected def connect(handler: Handler[P]): Unit
}

trait Listener[+P <: ProtocolCommon] extends ConnectionSetup[P] {
  final def startListening(failureReporter: Throwable => Unit = defaultReporter)
      (handler: Try[Connection[P]] => Unit): Try[Listening] =
    startListening(new Handler[P] {
      def notify(connection: Try[Connection[P]]) =
        try handler(connection)
        catch { case NonFatal(exception) => failureReporter(exception) }
    })

  protected def startListening(handler: Handler[P]): Try[Listening]

  final def firstConnection: Connector[P] = new Connector[P] {
    private val applyHandler = new AtomicBoolean(true)

    protected def connect(handler: Handler[P]): Unit = {
      var firstConnection: Connection[P] = null
      var listening: Try[Listening] = null

      listening = startListening() {
        case success @ Success(connection) =>
          if (applyHandler getAndSet false) {
            handler notify success
            connection.closed notify { _ =>
              if (listening != null)
                listening foreach { _.stopListening }
            }
            if (listening != null && !connection.open)
              listening foreach { _.stopListening }
            firstConnection = connection
          }
          else
              connection.close

        case _ =>
      }

      if (firstConnection != null && !firstConnection.open)
        listening foreach { _.stopListening }
    }
  }
}

trait Listening {
  def stopListening(): Unit
}
