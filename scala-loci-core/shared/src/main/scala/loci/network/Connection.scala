package loci
package network

import util.Notification

trait Connection {
  val receive: Notification[String]
  val closed: Notification[Unit]

  def send(data: String): Unit
  def close(): Unit
  def isOpen: Boolean
  def protocol: ProtocolInfo
}
