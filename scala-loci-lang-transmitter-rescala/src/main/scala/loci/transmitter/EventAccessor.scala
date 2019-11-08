package loci
package transmitter

import _root_.rescala.core.{ReSerializable, Scheduler, Struct}
import _root_.rescala.interface.RescalaInterface
import _root_.rescala.macros.cutOutOfUserComputation
import _root_.rescala.reactives.Event

protected[transmitter] trait EventAccessor {
  private final val asLocalId = 0
  private final val asLocalSeqId = 1

  implicit class RescalaEventMultipleAccessor[S <: Struct, V, R, T, L](
     value: V from R)(implicit
     ev: Transmission[V, R, Event[T, S], L, Multiple],
     val scheduler: Scheduler[S])
      extends RemoteAccessor {
    val interface = RescalaInterface.interfaceFor(scheduler)

    import interface.{Event, Evt, Signal, Var, transaction}

    @cutOutOfUserComputation lazy val asLocal: Signal[Seq[(Remote[R], Event[T])]] =
      value.cache(asLocalId) {
        implicit val serializer: ReSerializable[Seq[(Remote[R], Event[T])]] =
          ReSerializable.noSerializer

        val mapping = transaction() { _ => Var(Seq.empty[(Remote[R], Event[T])]) }

        def update() = mapping.set(value.remotes zip value.retrieveValues)

        value.remoteJoined foreach { _ => update() }
        value.remoteLeft foreach { _ => update() }
        update()

        mapping
      }

    @cutOutOfUserComputation lazy val asLocalFromAllSeq: Event[(Remote[R], T)] =
      value.cache(asLocalSeqId) {
        (asLocal map { remoteEvents =>
          (remoteEvents
            map { case (remote, event) => event map { (remote, _) } }
            reduceOption { _ || _ }
            getOrElse Evt[Nothing])
        }).flatten
      }
  }

  implicit class RescalaEventOptionalAccessor[S <: Struct, V, R, T, L](
     value: V from R)(implicit
     ev: Transmission[V, R, Event[T, S], L, Optional],
     val scheduler: Scheduler[S])
      extends RemoteAccessor {
    val interface = RescalaInterface.interfaceFor(scheduler)

    import interface.{Event, Evt, Signal, Var, transaction}

    @cutOutOfUserComputation lazy val asLocal: Signal[Option[Event[T]]] =
      value.cache(asLocalId) {
        implicit val serializer: ReSerializable[Option[Event[T]]] =
          ReSerializable.noSerializer

        val option = transaction() { _ => Var(Option.empty[Event[T]]) }

        def update() = option.set(value.retrieveValue)

        value.remoteJoined foreach { _ => update() }
        value.remoteLeft foreach { _ => update() }
        update()

        option
      }

    @cutOutOfUserComputation lazy val asLocalSeq: Event[T] =
      value.cache(asLocalSeqId) {
        (asLocal map { _ getOrElse Evt[Nothing] }).flatten
      }
  }

  implicit class RescalaEventSingleAccessor[S <: Struct, V, R, T, L](
     value: V from R)(implicit
     ev: Transmission[V, R, Event[T, S], L, Single],
     val scheduler: Scheduler[S])
      extends RemoteAccessor {
    val interface = RescalaInterface.interfaceFor(scheduler)

    import interface.Event

    @cutOutOfUserComputation lazy val asLocal: Event[T] =
      value.retrieveValue
  }
}
