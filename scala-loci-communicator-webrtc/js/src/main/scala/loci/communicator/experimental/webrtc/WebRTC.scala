package loci
package communicator
package experimental.webrtc

import org.scalajs.dom.experimental.webrtc._

import scala.scalajs.js.Array

trait WebRTC extends
    Protocol with
    SetupInfo with
    SecurityInfo with Secure with
    SymmetryInfo with Bidirectional {
  override def toString = "WebRTC()"
}

object WebRTC extends WebRTCUpdate {
  def unapply(webRTC: WebRTC) = true

  trait Connector extends communicator.Connector[WebRTC] {
    def use(update: IncrementalUpdate): Unit
    def set(update: CompleteUpdate): Unit
  }

  trait ConnectorFactory {
    def incremental(update: IncrementalUpdate => Unit): Connector
    def complete(update: CompleteSession => Unit): Connector
  }

  def apply(channel: RTCDataChannel): communicator.Connector[WebRTC] =
    new WebRTCChannelConnector(channel, None)

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
}
