package loci.transmitter.internal

object rescalaInterface {
  type Interface = rescala.operator.Interface
  val Pulse: rescala.structure.Pulse.type = rescala.structure.Pulse
  val default: rescala.default.type = rescala.default
}
