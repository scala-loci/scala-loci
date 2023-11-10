package loci
package transmitter

import _root_.rescala.interface.RescalaInterface
import loci.transmitter.internal.{RescalaEventTransmittable, RescalaSignalTransmittable}

class RescalaTransmitter[I <: RescalaInterface](val interface: I)
  extends RescalaSignalTransmittable with RescalaEventTransmittable

object rescala extends RescalaTransmitter[_root_.rescala.default.type](_root_.rescala.default)
