package retier
package ide.intellij

import typeconstraints._
import transmission._
import rescala.Event
import rescala.graph.Spores
import scala.language.implicitConversions
import scala.language.higherKinds

protected[retier] trait EventTransmissionProvider {
    this: rescalaTransmitter.type =>

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$retier$intellij$transmitMultipleRescalaEvent
    [V, T, U, S, P, Evt[U, ES <: Spores] <: Event[U, ES], L <: Peer, R <: Peer, ES <: Spores](v: V)
    (implicit
        dummy: IntelliJDummy,
        ev0: TransmissionProperties[V, T, R, L, MultipleConnection],
        ev1: T <:< Evt[U, ES],
        ev2: Transmittable[U, S, P],
        ev3: MultipleTransmission[Evt[P, ES], R, L] => RescalaEventMultipleTransmissionProvider[Evt, P, R, L, ES]):
    RescalaEventMultipleTransmissionProvider[Evt, P, R, L, ES] = ???

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$retier$intellij$transmitOptionalRescalaEvent
    [V, T, U, S, P, Evt[U, ES <: Spores] <: Event[U, ES], L <: Peer, R <: Peer, ES <: Spores](v: V)
    (implicit
        dummy: IntelliJDummy,
        ev0: TransmissionProperties[V, T, R, L, OptionalConnection],
        ev1: T <:< Evt[U, ES],
        ev2: Transmittable[U, S, P],
        ev3: OptionalTransmission[Evt[P, ES], R, L] => RescalaEventOptionalTransmissionProvider[Evt, P, R, L, ES]):
    RescalaEventOptionalTransmissionProvider[Evt, P, R, L, ES] = ???

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$retier$intellij$transmitSingleRescalaEvent
    [V, T, U, S, P, Evt[U, ES <: Spores] <: Event[U, ES], L <: Peer, R <: Peer, ES <: Spores](v: V)
    (implicit
        dummy: IntelliJDummy,
        ev0: TransmissionProperties[V, T, R, L, SingleConnection],
        ev1: T <:< Evt[U, ES],
        ev2: Transmittable[U, S, P],
        ev3: SingleTransmission[Evt[P, ES], R, L] => RescalaEventSingleTransmissionProvider[Evt, P, R, L, ES]):
    RescalaEventSingleTransmissionProvider[Evt, P, R, L, ES] = ???
}
