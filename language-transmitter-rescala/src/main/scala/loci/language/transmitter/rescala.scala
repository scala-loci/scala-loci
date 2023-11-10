package loci
package language
package transmitter

import _root_.rescala.interface.RescalaInterface
import loci.language.transmitter.internal.{RescalaEventAccessor, RescalaGateway, RescalaSignalAccessor}
import loci.transmitter.internal.{RescalaEventTransmittable, RescalaSignalTransmittable}

class RescalaTransmitter[I <: RescalaInterface](val interface: I) extends
  RescalaSignalTransmittable with
  RescalaEventTransmittable with
  RescalaSignalAccessor with
  RescalaEventAccessor with
  RescalaGateway

object rescala extends RescalaTransmitter[_root_.rescala.default.type](_root_.rescala.default)
