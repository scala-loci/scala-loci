package retier
package network

trait ProtocolInfo {
  def establisher: ConnectionEstablisher
  def establishedBy(establisher: ConnectionEstablisher): Boolean =
    establisher eq this.establisher

  def isEncrypted: Boolean
  def isProtected: Boolean
  def isAuthenticated: Boolean
  def identification: Option[Any]
}
