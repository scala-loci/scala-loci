package loci.dev
package language

sealed trait Gateway[+R]

object Gateway extends transmitter.RemoteGateway.Default {
  abstract class Implementation[+R] private[dev] extends Gateway[R]
}
