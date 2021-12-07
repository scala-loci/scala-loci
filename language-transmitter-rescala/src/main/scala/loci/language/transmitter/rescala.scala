package loci
package language
package transmitter

import _root_.rescala.interface.RescalaInterface

class RescalaTransmitter[I <: RescalaInterface](val interface: I) extends
  loci.transmitter.RescalaSignalTransmittable with
  loci.transmitter.RescalaEventTransmittable with
  RescalaSignalAccessor with
  RescalaEventAccessor with
  RescalaGateway

object rescala extends RescalaTransmitter[_root_.rescala.default.type](_root_.rescala.default)
