package loci
package communicator
package experimental.webrtc

import org.scalajs.dom.experimental.webrtc._

import scala.annotation.compileTimeOnly

trait WebRTC extends
    Protocol with
    SetupInfo with
    SecurityInfo with Secure with
    SymmetryInfo with Bidirectional {
  override def toString = "WebRTC()"
}

object WebRTC extends WebRTCUpdate {
  def unapply(webRTC: WebRTC) = true

  private def ??? =
    sys.error("WebRTC communicator only available in JS")

  trait Connector extends communicator.Connector[WebRTC] {
    def use(update: IncrementalUpdate): Unit
    def set(update: CompleteUpdate): Unit
  }

  trait ConnectorFactory {
    def incremental(update: IncrementalUpdate => Unit): Connector
    def complete(update: CompleteSession => Unit): Connector
  }

  @compileTimeOnly("WebRTC communicator only available in JS")
  def apply(channel: RTCDataChannel): communicator.Connector[WebRTC] = ???

  @compileTimeOnly("WebRTC communicator only available in JS")
  def offer(
    configuration: RTCConfiguration = RTCConfiguration(),
    options: RTCOfferOptions = RTCOfferOptions()): ConnectorFactory = ???

  @compileTimeOnly("WebRTC communicator only available in JS")
  def answer(
    configuration: RTCConfiguration = RTCConfiguration()): ConnectorFactory = ???
}
