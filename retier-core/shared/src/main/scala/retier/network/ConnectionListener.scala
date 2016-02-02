package retier
package network

import util.Notification
import util.Notifier

trait ConnectionListener { self =>
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

  def start(): Unit
  def stop(): Unit
}
