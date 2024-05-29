package loci
package communicator
package webrtc

import scala.annotation.compileTimeOnly

import org.scalajs.dom

protected[webrtc] trait WebRTCUpdate {
  sealed abstract class Update
  sealed abstract class IncrementalUpdate extends Update
  sealed abstract class CompleteUpdate extends Update

  sealed case class InitialSession(
    descType: String, sdp: String)
      extends IncrementalUpdate {

    @compileTimeOnly("WebRTC communicator only available in JS")
    def sessionDescription = WebRTC.unavailable
  }

  object InitialSession {
    @compileTimeOnly("WebRTC communicator only available in JS")
    def apply(value: dom.RTCSessionDescription): InitialSession = WebRTC.unavailable
  }

  sealed case class SessionUpdate(
    candidate: String, sdpMid: String, sdpMLineIndex: Double)
      extends IncrementalUpdate {
    @compileTimeOnly("WebRTC communicator only available in JS")
    def iceCandidate = WebRTC.unavailable
  }

  object SessionUpdate {
    @compileTimeOnly("WebRTC communicator only available in JS")
    def apply(value: dom.RTCIceCandidate): SessionUpdate = WebRTC.unavailable
  }

  sealed case class CompleteSession(
    descType: String, sdp: String)
      extends CompleteUpdate {
    @compileTimeOnly("WebRTC communicator only available in JS")
    def sessionDescription = WebRTC.unavailable
  }

  object CompleteSession {
    @compileTimeOnly("WebRTC communicator only available in JS")
    def apply(value: dom.RTCSessionDescription): CompleteSession = WebRTC.unavailable
  }
}
