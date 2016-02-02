package retier
package network

trait ProtocolInfo {
  def isEncrypted: Boolean
  def isProtected: Boolean
  def isAuthenticated: Boolean
  def identification: Option[Any]
}
