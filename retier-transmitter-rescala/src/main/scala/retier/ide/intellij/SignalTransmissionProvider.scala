package retier
package ide.intellij

import typeconstraints._
import transmission._
import rescala.Signal
import rescala.graph.Spores
import scala.language.implicitConversions
import scala.language.higherKinds

protected[retier] trait SignalTransmissionProvider {
    this: rescalaTransmitter.type =>

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$retier$intellij$transmitMultipleRescalaSignal
    [V, T, U, S, P, Sig[U, ES <: Spores] <: Signal[U, ES], L <: Peer, R <: Peer, ES <: Spores](v: V)
    (implicit
        dummy: IntelliJDummy,
        ev0: TransmissionProperties[V, T, R, L, MultipleConnection],
        ev1: T <:< Sig[U, ES],
        ev2: Transmittable[U, S, P],
        ev3: MultipleTransmission[Sig[P, ES], R, L] => RescalaSignalMultipleTransmissionProvider[Sig, P, R, L, ES]):
    RescalaSignalMultipleTransmissionProvider[Sig, P, R, L, ES] = ???

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$retier$intellij$transmitOptionalRescalaSignal
    [V, T, U, S, P, Sig[U, ES <: Spores] <: Signal[U, ES], L <: Peer, R <: Peer, ES <: Spores](v: V)
    (implicit
        dummy: IntelliJDummy,
        ev0: TransmissionProperties[V, T, R, L, OptionalConnection],
        ev1: T <:< Sig[U, ES],
        ev2: Transmittable[U, S, P],
        ev3: OptionalTransmission[Sig[P, ES], R, L] => RescalaSignalOptionalTransmissionProvider[Sig, P, R, L, ES]):
    RescalaSignalOptionalTransmissionProvider[Sig, P, R, L, ES] = ???

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$retier$intellij$transmitSingleRescalaSignalWithDefault
    [V, T, U, S, P, Sig[U, ES <: Spores] <: Signal[U, ES], L <: Peer, R <: Peer, ES <: Spores](v: V)
    (implicit
        dummy: IntelliJDummy,
        ev0: TransmissionProperties[V, T, R, L, SingleConnection],
        ev1: T <:< Sig[U, ES],
        ev2: SignalDefaultValue[U],
        ev3: Transmittable[U, S, P],
        ev4: SingleTransmission[Sig[P, ES], R, L] => RescalaSignalSingleTransmissionProviderWithDefaultValue[Sig, P, R, L, ES]):
    RescalaSignalSingleTransmissionProviderWithDefaultValue[Sig, P, R, L, ES] = ???

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$retier$intellij$transmitSingleRescalaSignalWithoutDefault
    [V, T, U, S, P, Sig[U, ES <: Spores] <: Signal[U, ES], L <: Peer, R <: Peer, ES <: Spores](v: V)
    (implicit
        dummy: IntelliJDummy,
        ev0: TransmissionProperties[V, T, R, L, SingleConnection],
        ev1: T <:< Sig[U, ES],
        ev2: NoSignalDefaultValue[U],
        ev3: Transmittable[U, S, P],
        ev4: SingleTransmission[Sig[P, ES], R, L] => RescalaSignalSingleTransmissionProviderWithoutDefaultValue[Sig, P, R, L, ES]):
    RescalaSignalSingleTransmissionProviderWithoutDefaultValue[Sig, P, R, L, ES] = ???
}
