package retier

object rescalaTransmitter extends
  ReactiveRemoteConnectionInterface with
  SignalTransmissionProvider with
  SignalTransmittable with
  EventTransmissionProvider with
  EventTransmittable with
  ide.intellij.ReactiveRemoteConnectionInterface with
  ide.intellij.SignalTransmissionProvider with
  ide.intellij.EventTransmissionProvider
