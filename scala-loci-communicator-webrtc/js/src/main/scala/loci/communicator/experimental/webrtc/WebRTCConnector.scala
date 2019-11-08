package loci
package communicator
package experimental.webrtc

import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.concurrent.duration._

import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer

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

private object WebRTCConnector {
  val channelLabel = "loci-webrtc-channel"
}

private abstract class WebRTCConnector(
  configuration: RTCConfiguration,
  update: Either[IncrementalUpdate => Unit, CompleteSession => Unit])
    extends WebRTC.Connector {

  val peerConnection = new RTCPeerConnection(configuration)

  peerConnection.onicecandidate = { event: RTCPeerConnectionIceEvent =>
    if (event.candidate != null)
      update.left foreach {
        _(SessionUpdate(event.candidate))
      }
    else
      update.right foreach {
        _(CompleteSession(peerConnection.localDescription))
      }
  }

  protected def handleConnectionClosing(connection: Try[Connection[WebRTC]]) = {
    connection match {
      case Success(connection) =>
        connection.closed foreach { _ => peerConnection.close }
      case _ =>
        peerConnection.close
    }
  }

  private var remoteDescriptionSet = false

  def use(update: IncrementalUpdate) = update match {
    case session: InitialSession =>
      if (!remoteDescriptionSet) {
        remoteDescriptionSet = true
        setRemoteDescription(session.sessionDescription)
      }
    case update: SessionUpdate =>
      peerConnection addIceCandidate update.iceCandidate
  }

  def set(update: CompleteUpdate) = update match {
    case session: CompleteSession =>
      if (!remoteDescriptionSet) {
        remoteDescriptionSet = true
        setRemoteDescription(session.sessionDescription)
      }
  }

  protected val unit = (): js.|[Unit, js.Thenable[Unit]]

  protected def setRemoteDescription(description: RTCSessionDescription): Unit
}

private class WebRTCOffer(
  configuration: RTCConfiguration,
  options: RTCOfferOptions,
  update: Either[IncrementalUpdate => Unit, CompleteSession => Unit])
    extends WebRTCConnector(configuration, update) {

  protected def connect(connectionEstablished: Connected[WebRTC]) = {
    val channel = peerConnection.createDataChannel(
      WebRTCConnector.channelLabel,
      RTCDataChannelInit())

    (peerConnection createOffer options) `then` { description: RTCSessionDescription =>
      (peerConnection setLocalDescription description) `then` { _: Unit =>
        update.left foreach { _(InitialSession(description)) }
        unit
      }
      unit
    }

    new WebRTCChannelConnector(channel, Some(this)).connect() { connection =>
      handleConnectionClosing(connection)
      connectionEstablished set connection
    }
  }

  protected def setRemoteDescription(description: RTCSessionDescription) =
    peerConnection setRemoteDescription description
}

private class WebRTCAnswer(
  configuration: RTCConfiguration,
  update: Either[IncrementalUpdate => Unit, CompleteSession => Unit])
    extends WebRTCConnector(configuration, update) {

  private var connector: Connector[WebRTC] = _
  private var connected: Connected[WebRTC] = _

  protected def connect(connectionEstablished: Connected[WebRTC]) =
    if (connector != null)
      connector.connect() { connection =>
        handleConnectionClosing(connection)
        connectionEstablished set connection
      }
    else
      connected = connectionEstablished

  peerConnection.ondatachannel = { event: RTCDataChannelEvent =>
    if (event.channel.label == WebRTCConnector.channelLabel && connector == null) {
      connector = new WebRTCChannelConnector(event.channel, Some(this))
      if (connected != null)
        connect(connected)
    }
  }

  protected def setRemoteDescription(description: RTCSessionDescription) =
    (peerConnection setRemoteDescription description) `then` { _: Unit =>
      peerConnection.createAnswer `then` { description: RTCSessionDescription =>
        (peerConnection setLocalDescription description) `then` { _: Unit =>
          update.left foreach { _(InitialSession(description)) }
          unit
        }
        unit
      }
      unit
    }
}

private class WebRTCChannelConnector(
  channel: RTCDataChannel,
  optionalConnectionSetup: Option[ConnectionSetup[WebRTC]])
    extends Connector[WebRTC] {

  protected def connect(connectionEstablished: Connected[WebRTC]) = {
    val reliable =
      (!(js isUndefined channel.asInstanceOf[js.Dynamic].reliable) &&
       (channel.asInstanceOf[js.Dynamic].reliable.asInstanceOf[Boolean])) ||
      ((js isUndefined channel.asInstanceOf[js.Dynamic].maxPacketLifeTime) &&
       (js isUndefined channel.asInstanceOf[js.Dynamic].maxRetransmits))

    if (reliable) {
      val connection = {
        val doClosed = Notice.Steady[Unit]
        val doReceive = Notice.Stream[MessageBuffer]

        val connection = new Connection[WebRTC] {
          val protocol = new WebRTC {
            val setup = optionalConnectionSetup getOrElse
              WebRTCChannelConnector.this
            val authenticated = false
          }

          val closed = doClosed.notice
          val receive = doReceive.notice

          var open = true
          def send(data: MessageBuffer) = channel send data.backingArrayBuffer
          def close() = if (open) {
            open = false
            channel.close
            doClosed.set()
          }
        }

        channel.onclose = { event: dom.Event =>
          connectionEstablished set Failure(new ConnectionException("channel closed"))
          connection.close
        }

        channel.onerror = { event: dom.Event =>
          connectionEstablished set Failure(new ConnectionException("channel closed"))
          connection.close
        }

        channel.onmessage = { event: dom.MessageEvent =>
          event.data match {
            case data: ArrayBuffer =>
              doReceive fire (MessageBuffer wrapArrayBuffer data)

            case data: dom.Blob =>
              val reader = new dom.FileReader
              reader.onload = { event: dom.Event =>
                doReceive fire (MessageBuffer wrapArrayBuffer
                  event.target.asInstanceOf[js.Dynamic].result.asInstanceOf[ArrayBuffer])
              }
              reader readAsArrayBuffer data

            case _ =>
          }
        }

        connection
      }

      channel.readyState match {
        case RTCDataChannelState.connecting =>
          // strange fix for strange issue with Chromium
          val handle = js.timers.setTimeout(1.day) { channel.readyState }
  
          channel.onopen = { _: dom.Event =>
            js.timers clearTimeout handle
            connectionEstablished set Success(connection)
          }
  
        case RTCDataChannelState.open =>
          connectionEstablished set Success(connection)

        case RTCDataChannelState.closing | RTCDataChannelState.closed =>
          connectionEstablished set Failure(new ConnectionException("channel closed"))
      }
    }
    else
      connectionEstablished set Failure(new ConnectionException("channel unreliable"))
  }
}
