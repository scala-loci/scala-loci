package retier
package network

import util.Notification

trait Connection {
  def protocol: ProtocolInfo
  def terminated: Notification[Unit]
  def isOpen: Boolean
  def close(): Unit
  def send(data: String): Unit
  def receive: Notification[String]
}
