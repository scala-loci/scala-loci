package retier
package experimental.webrtc

import network.ConnectionEstablisher
import network.ConnectionRequestor
import network.ProtocolInfo
import scala.scalajs.js.Array
import org.scalajs.dom.experimental.webrtc.RTCConfiguration
import org.scalajs.dom.experimental.webrtc.RTCOfferOptions
import org.scalajs.dom.experimental.webrtc.RTCDataChannel
import org.scalajs.dom.experimental.webrtc.RTCIceServer

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
  trait Connector extends ConnectionRequestor {
    def use(update: IncrementalUpdate): Unit
    def set(update: CompleteUpdate): Unit
  }

  trait ConnectorFactory {
    def incremental(update: IncrementalUpdate => Unit): Connector
    def complete(update: CompleteSession => Unit): Connector
  }

  def apply(channel: RTCDataChannel): ConnectionRequestor =
    new WebRTCChannelConnectionRequestor(channel, None)

  def offer(
      configuration: RTCConfiguration =
        RTCConfiguration(iceServers = Array[RTCIceServer]()),
      options: RTCOfferOptions =
        RTCOfferOptions()): ConnectorFactory =
    new ConnectorFactory {
      def incremental(update: IncrementalUpdate => Unit) =
        new WebRTCOffer(configuration, options, Left(update))
      def complete(update: CompleteSession => Unit) =
        new WebRTCOffer(configuration, options, Right(update))
    }

  def answer(
      configuration: RTCConfiguration =
        RTCConfiguration(iceServers = Array[RTCIceServer]())): ConnectorFactory =
    new ConnectorFactory {
      def incremental(update: IncrementalUpdate => Unit) =
        new WebRTCAnswer(configuration, Left(update))
      def complete(update: CompleteSession => Unit) =
        new WebRTCAnswer(configuration, Right(update))
    }

  def createProtocolInfo(
      establisher: ConnectionEstablisher,
      isEncrypted: Boolean, isProtected: Boolean, isAuthenticated: Boolean) =
    new WebRTC()(establisher, isEncrypted, isProtected, isAuthenticated) { }
}
