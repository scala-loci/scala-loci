package loci
package language

sealed trait Gateway[+R]

object Gateway extends transmitter.RemoteGateway.Default {
  abstract class Implementation[+R] private[loci] extends Gateway[R]
}
