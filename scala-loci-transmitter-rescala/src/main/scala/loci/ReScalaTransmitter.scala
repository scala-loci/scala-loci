package loci

object rescalaTransmitter extends
  ReactiveRemoteConnectionInterface with
  SignalTransmissionProvider with
  SignalTransmittable with
  EventTransmissionProvider with
  EventTransmittable with
  ide.intellij.ReactiveRemoteConnectionInterface with
  ide.intellij.SignalTransmissionProvider with
  ide.intellij.EventTransmissionProvider {

  class RemoteReactiveFailure(msg: String)
    extends IllegalStateException(msg)
}
