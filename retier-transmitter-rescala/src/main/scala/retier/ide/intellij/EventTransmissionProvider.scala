package retier
package ide.intellij

import typeconstraints._
import transmission._
import rescala.events.Event
import scala.language.implicitConversions
import scala.language.higherKinds

protected[retier] trait EventTransmissionProvider {
    this: rescalaTransmitter.type =>

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$retier$transmitMultipleRescalaEvent
    [V, T, U, S, P, Evt[U] <: Event[U], L <: Peer, R <: Peer](v: V)
    (implicit
        dummy: IntelliJDummy,
        ev0: TransmissionProperties[V, T, R, L, MultipleConnection],
        ev1: T <:< Evt[U],
        ev2: Transmittable[U, S, P],
        ev3: MultipleTransmission[Evt[P], R, L] => RescalaEventMultipleTransmissionProvider[Evt, P, R, L]):
    RescalaEventMultipleTransmissionProvider[Evt, P, R, L] = ???

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$retier$transmitOptionalRescalaEvent
    [V, T, U, S, P, Evt[U] <: Event[U], L <: Peer, R <: Peer](v: V)
    (implicit
        dummy: IntelliJDummy,
        ev0: TransmissionProperties[V, T, R, L, OptionalConnection],
        ev1: T <:< Evt[U],
        ev2: Transmittable[U, S, P],
        ev3: OptionalTransmission[Evt[P], R, L] => RescalaEventOptionalTransmissionProvider[Evt, P, R, L]):
    RescalaEventOptionalTransmissionProvider[Evt, P, R, L] = ???

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$retier$transmitSingleRescalaEvent
    [V, T, U, S, P, Evt[U] <: Event[U], L <: Peer, R <: Peer](v: V)
    (implicit
        dummy: IntelliJDummy,
        ev0: TransmissionProperties[V, T, R, L, SingleConnection],
        ev1: T <:< Evt[U],
        ev2: Transmittable[U, S, P],
        ev3: SingleTransmission[Evt[P], R, L] => RescalaEventSingleTransmissionProvider[Evt, P, R, L]):
    RescalaEventSingleTransmissionProvider[Evt, P, R, L] = ???
}
