package loci
package transmitter

object rescala extends SignalTransmittable with EventTransmittable {
  class RemoteReactiveFailure(msg: String)
    extends IllegalStateException(msg)
}
