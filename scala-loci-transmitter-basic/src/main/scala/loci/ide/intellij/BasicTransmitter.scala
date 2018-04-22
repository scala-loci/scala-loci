package loci
package ide.intellij

import typeconstraints._
import transmission._
import scala.language.implicitConversions

protected[loci] trait BasicTransmitter {
    this: basicTransmitter.type =>

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$loci$intellij$transmitMultipleBasic
    [V, T, S, U, L <: Peer, R <: Peer, Provider <: TransmissionProvider](v: V)
    (implicit
        dummy: IntelliJDummy,
        ev0: TransmissionProperties[V, T, R, L, MultipleTie],
        ev1: Transmittable[T, S, U],
        ev2: MultipleTransmission[U, R, L] => Provider,
        ev3: Provider <:< BasicMultipleTransmissionProvider[U, R, L]):
    BasicMultipleTransmissionProvider[U, R, L] = ???

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$loci$intellij$transmitOptionalBasic
    [V, T, S, U, L <: Peer, R <: Peer, Provider <: TransmissionProvider](v: V)
    (implicit
        dummy: IntelliJDummy,
        ev0: TransmissionProperties[V, T, R, L, OptionalTie],
        ev1: Transmittable[T, S, U],
        ev2: OptionalTransmission[U, R, L] => Provider,
        ev3: Provider <:< BasicOptionalTransmissionProvider[U, R, L]):
    BasicOptionalTransmissionProvider[U, R, L] = ???

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$loci$intellij$transmitSingleBasic
    [V, T, S, U, L <: Peer, R <: Peer, Provider <: TransmissionProvider]
    (v: V)
    (implicit
        dummy: IntelliJDummy,
        ev0: TransmissionProperties[V, T, R, L, SingleTie],
        ev1: Transmittable[T, S, U],
        ev2: SingleTransmission[U, R, L] => Provider,
        ev3: Provider <:< BasicSingleTransmissionProvider[U, R, L]):
    BasicSingleTransmissionProvider[U, R, L] = `#macro`
}
