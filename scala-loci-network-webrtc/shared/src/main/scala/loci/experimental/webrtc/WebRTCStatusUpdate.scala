package loci
package experimental.webrtc

import org.scalajs.dom.experimental.webrtc.RTCSdpType
import org.scalajs.dom.experimental.webrtc.RTCIceCandidate
import org.scalajs.dom.experimental.webrtc.RTCIceCandidateInit
import org.scalajs.dom.experimental.webrtc.RTCSessionDescription
import org.scalajs.dom.experimental.webrtc.RTCSessionDescriptionInit

protected[webrtc] trait WebRTCUpdate {
  sealed abstract class Update
  sealed abstract class IncrementalUpdate extends Update
  sealed abstract class CompleteUpdate extends Update

  sealed case class InitialSession(
    descType: String, sdp: String)
      extends IncrementalUpdate {
    def sessionDescription = new RTCSessionDescription(
      RTCSessionDescriptionInit(descType.asInstanceOf[RTCSdpType], sdp))
  }

  object InitialSession {
    def apply(value: RTCSessionDescription): InitialSession =
      InitialSession(value.`type`.asInstanceOf[String], value.sdp)
  }

  sealed case class SessionUpdate(
    candidate: String, sdpMid: String, sdpMLineIndex: Double)
      extends IncrementalUpdate {
    def iceCandidate = new RTCIceCandidate(
      RTCIceCandidateInit(candidate, sdpMid, sdpMLineIndex))
  }

  object SessionUpdate {
    def apply(value: RTCIceCandidate): SessionUpdate =
      SessionUpdate(value.candidate, value.sdpMid, value.sdpMLineIndex)
  }

  sealed case class CompleteSession(
    descType: String, sdp: String)
      extends CompleteUpdate {
    def sessionDescription = new RTCSessionDescription(
      RTCSessionDescriptionInit(descType.asInstanceOf[RTCSdpType], sdp))
  }

  object CompleteSession {
    def apply(value: RTCSessionDescription): CompleteSession =
      CompleteSession(value.`type`.asInstanceOf[String], value.sdp)
  }
}
