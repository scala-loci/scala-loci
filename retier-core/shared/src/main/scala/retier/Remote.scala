package retier

import network.ProtocolInfo
import util.Notification

trait Remote[+P <: Peer] extends Equals {
  def protocol: ProtocolInfo
  def isAuthenticated: Boolean
  def authenticate(): Unit
  def isConnected: Boolean
  def disconnect(): Unit
  val disconnected: Notification[Unit]
}
