package loci.transmitter.internal

object rescalaInterface {
  type Interface = rescala.interface.RescalaInterface
  val Pulse: rescala.operator.Pulse.type = rescala.operator.Pulse
  val default: rescala.default.type = rescala.default
}
