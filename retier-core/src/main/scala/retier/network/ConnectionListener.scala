package retier
package network

import util.Notification
import util.Notifier

trait ConnectionListener { self =>
  protected final val notifier = Notifier[Connection]

  final val connectionEstablished: Notification[Connection] =
    notifier.notification

  final def and(listener: ConnectionListener): ConnectionListener =
    new ConnectionListener {
      self.connectionEstablished += { notifier(_) }
      listener.connectionEstablished += { notifier(_) }

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
