package retier

import util.Attributes

case class Configuration(
  listeners: List[(PeerType, String, Attributes)],
  requestors: List[(PeerType, String, Attributes)])

trait ConfigurationParser {
  def load(configfile: String, peerType: PeerType, peerTypes: List[PeerType]):
    Configuration
}
