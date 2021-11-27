package loci
package transmitter
package transmittable

import _root_.rescala.interface.RescalaInterface

class RescalaTransmitter[I <: RescalaInterface](val interface: I)
  extends SignalTransmittable with EventTransmittable

object rescala extends RescalaTransmitter[_root_.rescala.default.type](_root_.rescala.default)
