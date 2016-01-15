package retier
package ide.intellij

import typeconstraints._
import transmission._
import scala.language.implicitConversions

protected[retier] trait BasicTransmitter {
    this: basicTransmitter.type =>

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$retier$transmitMultipleBasic
    [V, T, S, U, L <: Peer, R <: Peer, Provider <: TransmissionProvider](v: V)
    (implicit
        dummy: IntelliJDummy,
        ev0: TransmissionProperties[V, T, R, L, MultipleConnection],
        ev1: Transmittable[T, S, U],
        ev2: MultipleTransmission[U, R, L] => Provider,
        ev3: Provider <:< BasicMultipleTransmissionProvider[U, R, L]):
    BasicMultipleTransmissionProvider[U, R, L] = ???

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$retier$transmitOptionalBasic
    [V, T, S, U, L <: Peer, R <: Peer, Provider <: TransmissionProvider](v: V)
    (implicit
        dummy: IntelliJDummy,
        ev0: TransmissionProperties[V, T, R, L, OptionalConnection],
        ev1: Transmittable[T, S, U],
        ev2: OptionalTransmission[U, R, L] => Provider,
        ev3: Provider <:< BasicOptionalTransmissionProvider[U, R, L]):
    BasicOptionalTransmissionProvider[U, R, L] = ???

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$retier$transmitSingleBasic
    [V, T, S, U, L <: Peer, R <: Peer, Provider <: TransmissionProvider]
    (v: V)
    (implicit
        dummy: IntelliJDummy,
        ev0: TransmissionProperties[V, T, R, L, SingleConnection],
        ev1: Transmittable[T, S, U],
        ev2: SingleTransmission[U, R, L] => Provider,
        ev3: Provider <:< BasicSingleTransmissionProvider[U, R, L]):
    BasicSingleTransmissionProvider[U, R, L] = `#macro`
}
