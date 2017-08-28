package loci
package transmitter
package transmittable

object rescala extends SignalTransmittable with EventTransmittable {
  class RemoteReactiveFailure(msg: String)
    extends IllegalStateException(msg)
}
