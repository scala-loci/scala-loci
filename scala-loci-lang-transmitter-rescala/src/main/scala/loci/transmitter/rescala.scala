package loci
package transmitter

object rescala extends
  transmittable.SignalTransmittable with
  transmittable.EventTransmittable with
  SignalTransmissionProvider with
  EventTransmissionProvider with
  RescalaConnectionInterface
