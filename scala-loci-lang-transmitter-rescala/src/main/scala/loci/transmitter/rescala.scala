package loci
package transmitter

object rescala extends
  transmittable.SignalTransmittable with
  transmittable.EventTransmittable with
  SignalAccessor with
  EventAccessor with
  RescalaGateway
