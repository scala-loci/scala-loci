package loci
package ide.intellij

import typeconstraints._
import transmission._
import rescala.graph.Struct
import rescala.reactives.Signal
import scala.language.implicitConversions
import scala.language.higherKinds

protected[loci] trait SignalTransmissionProvider {
    this: rescalaTransmitter.type =>

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$loci$intellij$transmitMultipleRescalaSignal
    [V, T, U, S, P, Sig[U, ES <: Struct] <: Signal[U, ES], L <: Peer, R <: Peer, ES <: Struct](v: V)
    (implicit
        dummy: IntelliJDummy,
        ev0: TransmissionProperties[V, T, R, L, MultipleConnection],
        ev1: T <:< Sig[U, ES],
        ev2: Transmittable[U, S, P],
        ev3: MultipleTransmission[Sig[P, ES], R, L] => RescalaSignalMultipleTransmissionProvider[Sig, P, R, L, ES]):
    RescalaSignalMultipleTransmissionProvider[Sig, P, R, L, ES] = ???

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$loci$intellij$transmitOptionalRescalaSignal
    [V, T, U, S, P, Sig[U, ES <: Struct] <: Signal[U, ES], L <: Peer, R <: Peer, ES <: Struct](v: V)
    (implicit
        dummy: IntelliJDummy,
        ev0: TransmissionProperties[V, T, R, L, OptionalConnection],
        ev1: T <:< Sig[U, ES],
        ev2: Transmittable[U, S, P],
        ev3: OptionalTransmission[Sig[P, ES], R, L] => RescalaSignalOptionalTransmissionProvider[Sig, P, R, L, ES]):
    RescalaSignalOptionalTransmissionProvider[Sig, P, R, L, ES] = ???

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$loci$intellij$transmitSingleRescalaSignalWithDefault
    [V, T, U, S, P, Sig[U, ES <: Struct] <: Signal[U, ES], L <: Peer, R <: Peer, ES <: Struct](v: V)
    (implicit
        dummy: IntelliJDummy,
        ev0: TransmissionProperties[V, T, R, L, SingleConnection],
        ev1: T <:< Sig[U, ES],
        ev2: Transmittable[U, S, P],
        ev3: SingleTransmission[Sig[P, ES], R, L] => RescalaSignalSingleTransmissionProvider[Sig, P, R, L, ES]):
  RescalaSignalSingleTransmissionProvider[Sig, P, R, L, ES] = ???
}
