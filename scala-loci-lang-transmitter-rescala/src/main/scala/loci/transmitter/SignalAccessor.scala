package loci
package transmitter

import _root_.rescala.core.{Scheduler, Struct}
import _root_.rescala.macros.cutOutInReactiveMacro
import _root_.rescala.reactives.Signal

import scala.language.higherKinds

protected[transmitter] trait SignalAccessor {
  private final val asLocalId = 0

  implicit class RescalaSignalMultipleAccessor
      [Sig[T, St <: Struct] <: Signal[T, St], St <: Struct, V, R, T, L](
       value: V from R)(implicit
       ev: Transmission[V, R, Sig[T, St], L, Multiple],
       val scheduler: Scheduler[St])
    extends RemoteAccessor {
    import scheduler.{Signal, Var, transaction}

    lazy val asLocal: Signal[Seq[(Remote[R], Signal[T])]] @cutOutInReactiveMacro =
      value.cache(asLocalId) {
        val mapping = transaction() { _ => Var(Seq.empty[(Remote[R], Signal[T])]) }

        def update() = mapping.set(value.remotes zip value.retrieveValues)

        value.remoteJoined notify { _ => update() }
        value.remoteLeft notify { _ => update() }
        update()

        mapping
      }
  }

  implicit class RescalaOptionalMultipleAccessor
      [Sig[T, St <: Struct] <: Signal[T, St], St <: Struct, V, R, T, L](
       value: V from R)(implicit
       ev: Transmission[V, R, Sig[T, St], L, Optional],
       val scheduler: Scheduler[St])
    extends RemoteAccessor {
    import scheduler.{Signal, Var, transaction}

    lazy val asLocal: Signal[Option[T]] @cutOutInReactiveMacro =
      value.cache(asLocalId) {
        val option = transaction() { _ => Var(Option.empty[Signal[T]]) }

        def update() = option.set(value.retrieveValue)

        value.remoteJoined notify { _ => update() }
        value.remoteLeft notify { _ => update() }
        update()

        option.flatten
      }
  }

  implicit class RescalaSingleMultipleAccessor
      [Sig[T, St <: Struct] <: Signal[T, St], St <: Struct, V, R, T, L](
       value: V from R)(implicit
       ev: Transmission[V, R, Sig[T, St], L, Single],
       val scheduler: Scheduler[St])
    extends RemoteAccessor {
    import scheduler.Signal

    lazy val asLocal: Signal[T] @cutOutInReactiveMacro =
      value.retrieveValue
  }
}
