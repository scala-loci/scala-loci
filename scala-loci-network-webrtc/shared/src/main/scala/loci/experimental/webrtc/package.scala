package loci
package experimental

import loci.transmission.RemoteRef
import loci.transmission.PullBasedTransmittable

import webrtc.WebRTC.Update
import webrtc.WebRTC.IncrementalUpdate
import webrtc.WebRTC.CompleteUpdate
import webrtc.WebRTC.InitialSession
import webrtc.WebRTC.SessionUpdate
import webrtc.WebRTC.CompleteSession

package object webrtc {
  type UpdateRepresentation =
    ((String, String), (String, String), (String, String, Double))

  implicit object transmittableUpdate
    extends PullBasedTransmittable[
      Update, UpdateRepresentation, Update] {
    def send(value: Update, remote: RemoteRef) = value match {
      case value @ CompleteSession(_, _) =>
        (transmittableCompleteSession send (value, remote), null, null)
      case value @ InitialSession(_, _) =>
        (null, transmittableInitialSession send (value, remote), null)
      case value @ SessionUpdate(_, _, _) =>
        (null, null, transmittableSessionUpdate send (value, remote))
    }
    def receive(value: UpdateRepresentation, remote: RemoteRef) = value match {
      case (value, _, _) if value != null =>
        transmittableCompleteSession receive (value, remote)
      case (_, value, _) if value != null =>
        transmittableInitialSession receive (value, remote)
      case (_, _, value) if value != null =>
        transmittableSessionUpdate receive (value, remote)
    }
  }


  type IncrementalUpdateRepresentation =
    ((String, String), (String, String, Double))

  implicit object transmittableIncrementalUpdate
    extends PullBasedTransmittable[
      IncrementalUpdate, IncrementalUpdateRepresentation, IncrementalUpdate] {
    def send(value: IncrementalUpdate, remote: RemoteRef) = value match {
      case value @ InitialSession(_, _) =>
        (transmittableInitialSession send (value, remote), null)
      case value @ SessionUpdate(_, _, _) =>
        (null, transmittableSessionUpdate send (value, remote))
    }
    def receive(value: IncrementalUpdateRepresentation, remote: RemoteRef) = value match {
      case (value, _) if value != null =>
        transmittableInitialSession receive (value, remote)
      case (_, value) if value != null =>
        transmittableSessionUpdate receive (value, remote)
    }
  }


  type CompleteUpdateRepresentation = (String, String)

  implicit object transmittableCompleteUpdate
    extends PullBasedTransmittable[
      CompleteUpdate, CompleteUpdateRepresentation, CompleteUpdate] {
    def send(value: CompleteUpdate, remote: RemoteRef) = value match {
      case value @ CompleteSession(_, _) =>
        transmittableCompleteSession send (value, remote)
    }
    def receive(value: CompleteUpdateRepresentation, remote: RemoteRef) =
      transmittableCompleteSession receive (value, remote)
  }


  type InitialSessionRepresentation = (String, String)

  implicit object transmittableInitialSession
    extends PullBasedTransmittable[
      InitialSession, InitialSessionRepresentation, InitialSession] {
    def send(value: InitialSession, remote: RemoteRef) =
      (value.descType, value.sdp)
    def receive(value: InitialSessionRepresentation, remote: RemoteRef) =
      InitialSession(value._1, value._2)
  }


  type SessionUpdateRepresentation = (String, String, Double)

  implicit object transmittableSessionUpdate
    extends PullBasedTransmittable[
      SessionUpdate, SessionUpdateRepresentation, SessionUpdate] {
    def send(value: SessionUpdate, remote: RemoteRef) =
      (value.candidate, value.sdpMid, value.sdpMLineIndex)
    def receive(value: SessionUpdateRepresentation, remote: RemoteRef) =
      SessionUpdate(value._1, value._2, value._3)
  }


  type CompleteSessionRepresentation = (String, String)

  implicit object transmittableCompleteSession
    extends PullBasedTransmittable[
      CompleteSession, CompleteSessionRepresentation, CompleteSession] {
    def send(value: CompleteSession, remote: RemoteRef) =
      (value.descType, value.sdp)
    def receive(value: CompleteSessionRepresentation, remote: RemoteRef) =
      CompleteSession(value._1, value._2)
  }
}
