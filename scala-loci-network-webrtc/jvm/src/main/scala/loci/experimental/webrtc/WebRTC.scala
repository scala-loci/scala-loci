package loci
package experimental.webrtc

import network.ConnectionEstablisher
import network.ConnectionRequestor
import network.ProtocolInfo
import org.scalajs.dom.experimental.webrtc.RTCConfiguration
import org.scalajs.dom.experimental.webrtc.RTCOfferOptions
import org.scalajs.dom.experimental.webrtc.RTCDataChannel

abstract case class WebRTC private[WebRTC] ()(
    val establisher: ConnectionEstablisher,
    val isEncrypted: Boolean, val isProtected: Boolean, val isAuthenticated: Boolean)
  extends ProtocolInfo {

  private def readResolve(): Object =
    WebRTC.createProtocolInfo(establisher, isEncrypted, isProtected, isAuthenticated)
  def copy(): WebRTC =
    WebRTC.createProtocolInfo(establisher, isEncrypted, isProtected, isAuthenticated)

  val identification = None
}

object WebRTC extends WebRTCUpdate {
  private def jsOnly = sys.error(
      "A method of the WebRTC network provider has been called. " +
      "This is most likely because you tried to setup " +
      "the WebRTC network provider inside a Java Runtime Environment. " +
      "The WebRTC network provider only supports JavaScript.")

  trait Connector extends ConnectionRequestor {
    def use(update: IncrementalUpdate): Unit
    def set(update: CompleteUpdate): Unit
  }

  trait ConnectorFactory {
    def incremental(update: IncrementalUpdate => Unit): Connector
    def complete(update: CompleteSession => Unit): Connector
  }

  def apply(channel: RTCDataChannel): ConnectionRequestor =
    jsOnly

  def offer(
      configuration: RTCConfiguration = RTCConfiguration(),
      options: RTCOfferOptions = RTCOfferOptions()): ConnectorFactory =
    jsOnly

  def answer(
      configuration: RTCConfiguration = RTCConfiguration()): ConnectorFactory =
    jsOnly

  def createProtocolInfo(
      establisher: ConnectionEstablisher,
      isEncrypted: Boolean, isProtected: Boolean, isAuthenticated: Boolean) =
    new WebRTC()(establisher, isEncrypted, isProtected, isAuthenticated) { }
}
