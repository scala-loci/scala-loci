package loci
package experimental.webrtc

import network.Connection
import network.ConnectionRequestor
import util.Notifier
import contexts.Immediate.Implicits.global

import scala.util.Success
import scala.util.Failure
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.duration._

import scala.scalajs.js

import org.scalajs.dom
import org.scalajs.dom.experimental.webrtc.RTCConfiguration
import org.scalajs.dom.experimental.webrtc.RTCPeerConnection
import org.scalajs.dom.experimental.webrtc.RTCOfferOptions
import org.scalajs.dom.experimental.webrtc.RTCDataChannel
import org.scalajs.dom.experimental.webrtc.RTCDataChannelInit
import org.scalajs.dom.experimental.webrtc.RTCDataChannelState
import org.scalajs.dom.experimental.webrtc.RTCDataChannelEvent
import org.scalajs.dom.experimental.webrtc.RTCSessionDescription
import org.scalajs.dom.experimental.webrtc.RTCPeerConnectionIceEvent

import WebRTC.IncrementalUpdate
import WebRTC.CompleteUpdate
import WebRTC.InitialSession
import WebRTC.SessionUpdate
import WebRTC.CompleteSession

private object WebRTCConnectionRequestor {
  val channelLabel = "loci-webrtc-channel"
}

private abstract class WebRTCConnectionRequestor(
  configuration: RTCConfiguration,
  update: Either[IncrementalUpdate => Unit, CompleteSession => Unit])
    extends WebRTC.Connector {
  val peerConnection = new RTCPeerConnection(configuration)

  peerConnection.onicecandidate = { event: RTCPeerConnectionIceEvent =>
    if (event.candidate != null)
      update.left foreach { _(SessionUpdate(event.candidate)) }
    else
      update.right foreach {
        _(CompleteSession(peerConnection.localDescription))
      }
  }

  protected def propagateConnectionClosing(connection: Future[Connection]) = {
    connection onComplete {
      case Success(connection) =>
        connection.closed += { _ => peerConnection.close }
      case _ =>
        peerConnection.close
    }
    connection
  }

  private val promise = Promise[RTCSessionDescription]
  promise.future foreach setRemoteDescription

  def use(update: IncrementalUpdate) = update match {
    case session: InitialSession =>
      promise trySuccess session.sessionDescription
    case update: SessionUpdate =>
      peerConnection addIceCandidate update.iceCandidate
  }

  def set(update: CompleteUpdate) = update match {
    case session: CompleteSession =>
      promise trySuccess session.sessionDescription
  }

  protected def setRemoteDescription(description: RTCSessionDescription): Unit
}

private class WebRTCOffer(
  configuration: RTCConfiguration,
  options: RTCOfferOptions,
  update: Either[IncrementalUpdate => Unit, CompleteSession => Unit])
    extends WebRTCConnectionRequestor(configuration, update) {
  protected def setRemoteDescription(description: RTCSessionDescription) =
    peerConnection setRemoteDescription description

  def request = {
    val channel = peerConnection.createDataChannel(
      WebRTCConnectionRequestor.channelLabel,
      RTCDataChannelInit())
    val requestor = new WebRTCChannelConnectionRequestor(channel, Some(this))
    val request = propagateConnectionClosing(requestor.request)

    request.value match {
      case Some(Failure(exception)) =>
        Future failed exception
      case _ =>
        (peerConnection createOffer options).toFuture flatMap { description =>
          (peerConnection setLocalDescription description).toFuture flatMap { _ =>
            update.left foreach { _(InitialSession(description)) }
            request
          }
        }
    }
  }
}

private class WebRTCAnswer(
  configuration: RTCConfiguration,
  update: Either[IncrementalUpdate => Unit, CompleteSession => Unit])
    extends WebRTCConnectionRequestor(configuration, update) {
  protected def setRemoteDescription(description: RTCSessionDescription) =
    (peerConnection setRemoteDescription description).toFuture foreach { _ =>
      peerConnection.createAnswer.toFuture foreach { description =>
        (peerConnection setLocalDescription description).toFuture foreach { _ =>
          update.left foreach { _(InitialSession(description)) }
        }
      }
    }

  private val promise = Promise[WebRTCChannelConnectionRequestor]

  peerConnection.ondatachannel = { event: RTCDataChannelEvent =>
    if (event.channel.label == WebRTCConnectionRequestor.channelLabel)
      promise trySuccess new WebRTCChannelConnectionRequestor(
        event.channel, Some(this))
  }

  def request = promise.future flatMap { requestor =>
    propagateConnectionClosing(requestor.request)
  }
}

private class WebRTCChannelConnectionRequestor(
  channel: RTCDataChannel,
  establisher: Option[ConnectionRequestor])
    extends ConnectionRequestor {
  private val promise = Promise[Connection]

  if (((js isUndefined channel.asInstanceOf[js.Dynamic].reliable) ||
       (!channel.asInstanceOf[js.Dynamic].reliable.asInstanceOf[Boolean])) &&
      (!(js isUndefined channel.asInstanceOf[js.Dynamic].maxPacketLifeTime) ||
       !(js isUndefined channel.asInstanceOf[js.Dynamic].maxRetransmits)))
    promise failure new RemoteConnectionException("channel unreliable")

  if (!promise.isCompleted) {
    val connection = {
      val doClosed = Notifier[Unit]
      val doReceive = Notifier[String]

      val connection = new Connection {
        val protocol = WebRTC.createProtocolInfo(
          establisher getOrElse WebRTCChannelConnectionRequestor.this,
          isEncrypted = true, isProtected = true, isAuthenticated = false)
        val closed = doClosed.notification
        val receive = doReceive.notification

        var isOpen = true
        def send(data: String) = channel send data
        def close() = if (isOpen) {
          isOpen = false
          channel.close
          doClosed()
        }
      }

      channel.onclose = { event: dom.Event =>
        promise tryFailure new RemoteConnectionException("channel closed")
        connection.close
      }

      channel.onerror = { event: dom.Event =>
        promise tryFailure new RemoteConnectionException("channel closed")
        connection.close
      }

      channel.onmessage = { event: dom.MessageEvent =>
        event.data match {
          case data: String => doReceive(data)
          case _ =>
        }
      }

      connection
    }

    channel.readyState match {
      case RTCDataChannelState.connecting =>
        // strange fix for strange issue with Cromium
        val handle = js.timers.setTimeout(1.day) { channel.readyState }

        channel.onopen = { _: dom.Event =>
          js.timers clearTimeout handle
          promise trySuccess connection
        }

      case RTCDataChannelState.open =>
        promise trySuccess connection

      case RTCDataChannelState.closing | RTCDataChannelState.closed =>
        promise tryFailure new RemoteConnectionException("channel closed")
    }
  }

  def request = promise.future
}
