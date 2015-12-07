package retier
package ide.intellij

import typeconstraints._
import transmission._
import scala.language.implicitConversions

protected[retier] trait BasicSyncTransmitter {
    this: basicSyncTransmitter.type =>

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$retier$transmitMultipleBasicAsync
    [V, T, S, U, L <: Peer, R <: Peer, Provider <: TransmissionProvider](v: V)
    (implicit
        dummy: IntelliJDummy,
        ev0: TransmissionProperties[V, T, R, L, MultipleConnection],
        ev1: Transmittable[T, S, U],
        ev2: MultipleTransmission[U, R, L] => Provider,
        ev3: Provider <:< BasicSyncMultipleTransmissionProvider[U, R, L]):
    BasicSyncMultipleTransmissionProvider[U, R, L] = ???

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$retier$transmitOptionalBasicAsync
    [V, T, S, U, L <: Peer, R <: Peer, Provider <: TransmissionProvider](v: V)
    (implicit
        dummy: IntelliJDummy,
        ev0: TransmissionProperties[V, T, R, L, OptionalConnection],
        ev1: Transmittable[T, S, U],
        ev2: OptionalTransmission[U, R, L] => Provider,
        ev3: Provider <:< BasicSyncOptionalTransmissionProvider[U, R, L]):
    BasicSyncOptionalTransmissionProvider[U, R, L] = ???

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$retier$transmitSingleBasicAsync
    [V, T, S, U, L <: Peer, R <: Peer, Provider <: TransmissionProvider]
    (v: V)
    (implicit
        dummy: IntelliJDummy,
        ev0: TransmissionProperties[V, T, R, L, SingleConnection],
        ev1: Transmittable[T, S, U],
        ev2: SingleTransmission[U, R, L] => Provider,
        ev3: Provider <:< BasicSyncSingleTransmissionProvider[U, R, L]):
    BasicSyncSingleTransmissionProvider[U, R, L] = `#macro`
}
