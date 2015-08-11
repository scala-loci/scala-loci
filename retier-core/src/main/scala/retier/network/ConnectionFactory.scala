package retier
package network

import util.Attributes

trait ConnectionFactory { self =>
  final def and(factory: ConnectionFactory): ConnectionFactory =
    new ConnectionFactory {
      def listener(url: String, attrs: Attributes) =
        self.listener(url, attrs) orElse factory.listener(url, attrs)
      def requestor(config: String, attrs: Attributes) =
        self.requestor(config, attrs) orElse factory.requestor(config, attrs)
    }

  def listener(url: String, attrs: Attributes): Option[ConnectionListener]
  def requestor(config: String, attrs: Attributes): Option[ConnectionRequestor]
}
