package loci
package transmitter

import _root_.rescala.core.{Scheduler, Struct}
import _root_.rescala.macros.cutOutInReactiveMacro
import _root_.rescala.reactives.Event

import scala.language.higherKinds

protected[transmitter] trait EventAccessor {
  private final val asLocalId = 0
  private final val asLocalSeqId = 1

  implicit class RescalaEventMultipleAccessor
      [Evt[T, St <: Struct] <: Event[T, St], St <: Struct, V, R, T, L](
       value: V from R)(implicit
       ev: Transmission[V, R, Evt[T, St], L, Multiple],
       val scheduler: Scheduler[St])
    extends RemoteAccessor {
    import scheduler.{Event, Evt, Signal, Var, transaction}

    lazy val asLocal: Signal[Seq[(Remote[R], Event[T])]] @cutOutInReactiveMacro =
      value.cache(asLocalId) {
        val mapping = transaction() { _ => Var(Seq.empty[(Remote[R], Event[T])]) }

        def update() = mapping.set(value.remotes zip value.retrieveValues)

        value.remoteJoined notify { _ => update() }
        value.remoteLeft notify { _ => update() }
        update()

        mapping
      }

    lazy val asLocalFromAllSeq: Event[(Remote[R], T)] @cutOutInReactiveMacro =
      value.cache(asLocalSeqId) {
        (asLocal map { remoteEvents =>
          (remoteEvents
            map { case (remote, event) => event map { (remote, _) } }
            reduceOption { _ || _ }
            getOrElse Evt[Nothing])
        }).flatten
      }
  }

  implicit class RescalaEventOptionalAccessor
      [Evt[T, St <: Struct] <: Event[T, St], St <: Struct, V, R, T, L](
       value: V from R)(implicit
       ev: Transmission[V, R, Evt[T, St], L, Optional],
       val scheduler: Scheduler[St])
    extends RemoteAccessor {
    import scheduler.{Event, Evt, Signal, Var, transaction}

    lazy val asLocal: Signal[Option[Event[T]]] @cutOutInReactiveMacro =
      value.cache(asLocalId) {
        val option = transaction() { _ => Var(Option.empty[Event[T]]) }

        def update() = option.set(value.retrieveValue)

        value.remoteJoined notify { _ => update() }
        value.remoteLeft notify { _ => update() }
        update()

        option
      }

    lazy val asLocalSeq: Event[T] @cutOutInReactiveMacro =
      value.cache(asLocalSeqId) {
        (asLocal map { _ getOrElse Evt[Nothing] }).flatten
      }
  }

  implicit class RescalaEventSingleAccessor
      [Evt[T, St <: Struct] <: Event[T, St], St <: Struct, V, R, T, L](
       value: V from R)(implicit
       ev: Transmission[V, R, Evt[T, St], L, Single],
       val scheduler: Scheduler[St])
    extends RemoteAccessor {
    import scheduler.Event

    lazy val asLocal: Event[T] @cutOutInReactiveMacro =
      value.retrieveValue
  }
}
