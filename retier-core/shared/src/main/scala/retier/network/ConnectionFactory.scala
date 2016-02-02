package retier
package network

import util.Attributes

trait ConnectionFactory { self =>
  final def and(factory: ConnectionFactory): ConnectionFactory =
    new ConnectionFactory {
      def listener(config: String, attrs: Attributes) =
        self.listener(config, attrs) orElse factory.listener(config, attrs)
      def requestor(url: String, attrs: Attributes) =
        self.requestor(url, attrs) orElse factory.requestor(url, attrs)
    }

  def listener(config: String, attrs: Attributes): Option[ConnectionListener]
  def requestor(url: String, attrs: Attributes): Option[ConnectionRequestor]
}
