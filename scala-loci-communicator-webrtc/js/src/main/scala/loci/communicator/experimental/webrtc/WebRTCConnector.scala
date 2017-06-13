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
        connection.closed notify { _ => peerConnection.close }
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

  protected def connect(handler: Handler[WebRTC]) = {
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
      handler notify connection
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
  private var connectionHandler: Handler[WebRTC] = _

  protected def connect(handler: Handler[WebRTC]) =
    if (connector != null)
      connector.connect() { connection =>
        handleConnectionClosing(connection)
        handler notify connection
      }
    else
      connectionHandler = handler

  peerConnection.ondatachannel = { event: RTCDataChannelEvent =>
    if (event.channel.label == WebRTCConnector.channelLabel && connector == null) {
      connector = new WebRTCChannelConnector(event.channel, Some(this))
      if (connectionHandler != null)
        connect(connectionHandler)
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

  protected def connect(handler: Handler[WebRTC]) = {
    val reliable =
      (!(js isUndefined channel.asInstanceOf[js.Dynamic].reliable) &&
       (channel.asInstanceOf[js.Dynamic].reliable.asInstanceOf[Boolean])) ||
      ((js isUndefined channel.asInstanceOf[js.Dynamic].maxPacketLifeTime) &&
       (js isUndefined channel.asInstanceOf[js.Dynamic].maxRetransmits))

    if (reliable) {
      val connection = {
        val doClosed = Notifier[Unit]
        val doReceive = Notifier[MessageBuffer]

        val connection = new Connection[WebRTC] {
          val protocol = new WebRTC {
            val setup = optionalConnectionSetup getOrElse
              WebRTCChannelConnector.this
            val authenticated = false
          }

          val closed = doClosed.notification
          val receive = doReceive.notification

          var open = true
          def send(data: MessageBuffer) = channel send data.backingArrayBuffer
          def close() = if (open) {
            open = false
            channel.close
            doClosed()
          }
        }

        channel.onclose = { event: dom.Event =>
          handler notify Failure(new ConnectionException("channel closed"))
          connection.close
        }

        channel.onerror = { event: dom.Event =>
          handler notify Failure(new ConnectionException("channel closed"))
          connection.close
        }

        channel.onmessage = { event: dom.MessageEvent =>
          event.data match {
            case data: ArrayBuffer =>
              doReceive(MessageBuffer wrapArrayBuffer data)

            case data: dom.Blob =>
              val reader = new dom.FileReader
              reader.onload = { event: dom.Event =>
                doReceive(MessageBuffer wrapArrayBuffer
                  event.target.asInstanceOf[js.Dynamic]
                       .result.asInstanceOf[ArrayBuffer])
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
            handler notify Success(connection)
          }
  
        case RTCDataChannelState.open =>
          handler notify Success(connection)

        case RTCDataChannelState.closing | RTCDataChannelState.closed =>
          handler notify Failure(new ConnectionException("channel closed"))
      }
    }
    else
      handler notify Failure(new ConnectionException("channel unreliable"))
  }
}
