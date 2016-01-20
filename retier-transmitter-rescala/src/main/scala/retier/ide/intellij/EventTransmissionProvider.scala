package retier
package ide.intellij

import typeconstraints._
import transmission._
import rescala.synchronization.Engines.default
import rescala.synchronization.Engines.default._
import scala.language.implicitConversions
import scala.language.higherKinds

protected[retier] trait EventTransmissionProvider {
    this: rescalaTransmitter.type =>

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$retier$intellij$transmitMultipleRescalaEvent
    [V, T, U, S, P, Evnt[U] <: Event[U], L <: Peer, R <: Peer](v: V)
    (implicit
        dummy: IntelliJDummy,
        ev0: TransmissionProperties[V, T, R, L, MultipleConnection],
        ev1: T <:< Evnt[U],
        ev2: Transmittable[U, S, P],
        ev3: MultipleTransmission[Evnt[P], R, L] => RescalaEventMultipleTransmissionProvider[Evnt, P, R, L]):
    RescalaEventMultipleTransmissionProvider[Evnt, P, R, L] = ???

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$retier$intellij$transmitOptionalRescalaEvent
    [V, T, U, S, P, Evnt[U] <: Event[U], L <: Peer, R <: Peer](v: V)
    (implicit
        dummy: IntelliJDummy,
        ev0: TransmissionProperties[V, T, R, L, OptionalConnection],
        ev1: T <:< Evnt[U],
        ev2: Transmittable[U, S, P],
        ev3: OptionalTransmission[Evnt[P], R, L] => RescalaEventOptionalTransmissionProvider[Evnt, P, R, L]):
    RescalaEventOptionalTransmissionProvider[Evnt, P, R, L] = ???

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$retier$intellij$transmitSingleRescalaEvent
    [V, T, U, S, P, Evnt[U] <: Event[U], L <: Peer, R <: Peer](v: V)
    (implicit
        dummy: IntelliJDummy,
        ev0: TransmissionProperties[V, T, R, L, SingleConnection],
        ev1: T <:< Evnt[U],
        ev2: Transmittable[U, S, P],
        ev3: SingleTransmission[Evnt[P], R, L] => RescalaEventSingleTransmissionProvider[Evnt, P, R, L]):
    RescalaEventSingleTransmissionProvider[Evnt, P, R, L] = ???
}
