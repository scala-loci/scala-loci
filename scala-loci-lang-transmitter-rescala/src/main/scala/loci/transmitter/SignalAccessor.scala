package loci
package transmitter

import _root_.rescala.core.{ReSerializable, Scheduler, Struct}
import _root_.rescala.interface.RescalaInterface
import _root_.rescala.macros.cutOutOfUserComputation
import _root_.rescala.reactives.Signal

protected[transmitter] trait SignalAccessor {
  private final val asLocalId = 0

  implicit class RescalaSignalMultipleAccessor[S <: Struct, V, R, T, L](
    value: V from R)(implicit
    ev: Transmission[V, R, Signal[T, S], L, Multiple],
    val scheduler: Scheduler[S])
      extends RemoteAccessor {
    val interface = RescalaInterface.interfaceFor(scheduler)

    import interface.{Signal, Var, transaction}

    @cutOutOfUserComputation lazy val asLocalFromAll: Signal[Seq[(Remote[R], Signal[T])]] =
      value.cache(asLocalId) {
        implicit val serializer = ReSerializable.noSerializer[Seq[(Remote[R], Signal[T])]]

        val mapping = transaction() { _ => Var(Seq.empty[(Remote[R], Signal[T])]) }

        def update() = mapping.set(value.remotes zip value.retrieveValues)

        value.remoteJoined foreach { _ => update() }
        value.remoteLeft foreach { _ => update() }
        update()

        mapping
      }
  }

  implicit class RescalaSignalOptionalAccessor[S <: Struct, V, R, T, L](
    value: V from R)(implicit
    ev: Transmission[V, R, Signal[T, S], L, Optional],
    val scheduler: Scheduler[S])
      extends RemoteAccessor {
    val interface = RescalaInterface.interfaceFor(scheduler)

    import interface.{Signal, Var, transaction}

    @cutOutOfUserComputation lazy val asLocal: Signal[Option[T]] =
      value.cache(asLocalId) {
        implicit val serializer = ReSerializable.noSerializer[Option[Signal[T]]]

        val option = transaction() { _ => Var(Option.empty[Signal[T]]) }

        def update() = option.set(value.retrieveValue)

        value.remoteJoined foreach { _ => update() }
        value.remoteLeft foreach { _ => update() }
        update()

        option.flatten
      }
  }

  implicit class RescalaSignalSingleAccessor[S <: Struct, V, R, T, L](
    value: V from R)(implicit
    ev: Transmission[V, R, Signal[T, S], L, Single],
    val scheduler: Scheduler[S])
      extends RemoteAccessor {
    val interface = RescalaInterface.interfaceFor(scheduler)

    import interface.Signal

    @cutOutOfUserComputation lazy val asLocal: Signal[T] =
      value.retrieveValue
  }
}
