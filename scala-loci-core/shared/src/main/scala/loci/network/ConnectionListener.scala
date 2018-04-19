package loci
package network

import util.Notification
import util.Notifier

import scala.concurrent.Promise

trait ConnectionListener extends ConnectionEstablisher { self =>
  protected final val doConnectionEstablished = Notifier[Connection]

  final val connectionEstablished: Notification[Connection] =
    doConnectionEstablished.notification

  final def and(listener: ConnectionListener): ConnectionListener =
    new ConnectionListener {
      self.connectionEstablished += { doConnectionEstablished(_) }
      listener.connectionEstablished += { doConnectionEstablished(_) }

      def start = {
        self.start()
        listener.start()
      }

      def stop = {
        self.stop()
        listener.stop()
      }
    }

  final def firstRequest: ConnectionRequestor =
    new ConnectionRequestor {
      private val lock = new Object

      def request = {
        val promise = Promise[Connection]

        connectionEstablished += { connection =>
          lock synchronized {
            if (!promise.isCompleted) {
              connection.closed += { _ => self.stop() }
              if (!connection.isOpen)
                self.stop()
              promise success connection
            }
            else
              connection.close()
          }
        }

        self.start()

        promise.future
      }
    }

  def start(): Unit
  def stop(): Unit
}
