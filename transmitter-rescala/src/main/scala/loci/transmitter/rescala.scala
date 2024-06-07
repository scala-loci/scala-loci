package loci
package transmitter

import loci.transmitter.internal.rescalaInterface._
import loci.transmitter.internal.{RescalaEventTransmittable, RescalaSignalTransmittable}

class RescalaTransmitter[I <: Interface](val interface: I)
  extends RescalaSignalTransmittable with RescalaEventTransmittable

object rescala extends RescalaTransmitter[default.type](default)
