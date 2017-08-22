package loci

import communicator.ConnectionSetupFactory

case class Configuration(
  listeners: List[(PeerType, String, ConnectionSetupFactory.Properties)],
  connectors: List[(PeerType, String, ConnectionSetupFactory.Properties)])

trait ConfigurationParser {
  def load(configfile: String, peerType: PeerType, peerTypes: List[PeerType]):
    Configuration
}
