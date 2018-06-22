package loci
package communicator
package experimental

import loci.transmitter.TransformingTransmittable

import webrtc.WebRTC.Update
import webrtc.WebRTC.IncrementalUpdate
import webrtc.WebRTC.CompleteUpdate
import webrtc.WebRTC.InitialSession
import webrtc.WebRTC.SessionUpdate
import webrtc.WebRTC.CompleteSession

package object webrtc {
  type UpdateRepresentation =
    ((String, String), (String, String), (String, String, Double))

  implicit val transmittableUpdate: TransformingTransmittable[
      Update, UpdateRepresentation, Update] =
    TransformingTransmittable(
      provide = (value, context) => value match {
        case value @ CompleteSession(descType, sdp) =>
          ((descType, sdp), null, null)
        case value @ InitialSession(descType, sdp) =>
          (null, (descType, sdp), null)
        case value @ SessionUpdate(candidate, sdpMid, sdpMLineIndex) =>
          (null, null, (candidate, sdpMid, sdpMLineIndex))
      },
      receive = (value, context) => value match {
        case ((descType, sdp), _, _) =>
          CompleteSession(descType, sdp)
        case (_, (descType, sdp), _) if value != null =>
          InitialSession(descType, sdp)
        case (_, _, (candidate, sdpMid, sdpMLineIndex)) if value != null =>
          SessionUpdate(candidate, sdpMid, sdpMLineIndex)
      })


  type IncrementalUpdateRepresentation =
    ((String, String), (String, String, Double))

  implicit val transmittableIncrementalUpdate: TransformingTransmittable[
      IncrementalUpdate, IncrementalUpdateRepresentation, IncrementalUpdate] =
    TransformingTransmittable(
      provide = (value, context) => value match {
        case value @ InitialSession(descType, sdp) =>
          ((descType, sdp), null)
        case value @ SessionUpdate(candidate, sdpMid, sdpMLineIndex) =>
          (null, (candidate, sdpMid, sdpMLineIndex))
      },
      receive = (value, context) => value match {
        case ((descType, sdp), _) if value != null =>
          InitialSession(descType, sdp)
        case (_, (candidate, sdpMid, sdpMLineIndex)) if value != null =>
          SessionUpdate(candidate, sdpMid, sdpMLineIndex)
      })


  type CompleteUpdateRepresentation = (String, String)

  implicit val transmittableCompleteUpdate: TransformingTransmittable[
      CompleteUpdate, CompleteUpdateRepresentation, CompleteUpdate] =
    TransformingTransmittable(
      provide = (value, context) => value match {
        case value @ CompleteSession(descType, sdp) => (descType, sdp)
      },
      receive = (value, context) =>
        CompleteSession(value._1, value._2))


  type InitialSessionRepresentation = (String, String)

  implicit val transmittableInitialSession: TransformingTransmittable[
    InitialSession, InitialSessionRepresentation, InitialSession] =
    TransformingTransmittable(
      provide = (value, context) =>
        (value.descType, value.sdp),
      receive = (value, context) =>
        InitialSession(value._1, value._2))


  type SessionUpdateRepresentation = (String, String, Double)

  implicit val transmittableSessionUpdate: TransformingTransmittable[
      SessionUpdate, SessionUpdateRepresentation, SessionUpdate] =
    TransformingTransmittable(
      provide = (value, context) =>
        (value.candidate, value.sdpMid, value.sdpMLineIndex),
      receive = (value, context) =>
        SessionUpdate(value._1, value._2, value._3))


  type CompleteSessionRepresentation = (String, String)

  implicit val transmittableCompleteSession: TransformingTransmittable[
      CompleteSession, CompleteSessionRepresentation, CompleteSession] =
    TransformingTransmittable(
      provide = (value, context) =>
        (value.descType, value.sdp),
      receive = (value, context) =>
        CompleteSession(value._1, value._2))
}
