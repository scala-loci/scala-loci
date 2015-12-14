package retier

import network.ProtocolInfo
import util.Notification

trait Remote[+P <: Peer] extends Any with Equals {
  def protocol: ProtocolInfo
  def disconnected: Notification[Unit]
  def isConnected: Boolean
  def isAuthenticated: Boolean
  def authenticate(): Unit
}
