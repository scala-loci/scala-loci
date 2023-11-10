package loci.language.transmitter.internal

import _root_.rescala.interface.RescalaInterface
import _root_.rescala.operator.cutOutOfUserComputation
import loci.language.Remote
import loci.language.transmitter.{Multiple, Optional, RemoteAccessor, Single, Transmission, from}

private[loci] trait RescalaEventAccessor {
  val interface: RescalaInterface
  import interface._

  private final val asLocalId = 0
  private final val asLocalSeqId = 1

  implicit class RescalaEventMultipleAccessor[V, R, T, L](
    value: V from R)(implicit
    ev: Transmission[V, R, Event[T], L, Multiple])
      extends RemoteAccessor {

    @cutOutOfUserComputation lazy val asLocalFromAll: Signal[Seq[(Remote[R], Event[T])]] =
      value.cache(asLocalId) {
        val mapping = transaction() { _ => Var(Seq.empty[(Remote[R], Event[T])]) }

        def update() = mapping.set(value.remotes zip value.retrieveValues)

        value.remoteJoined foreach { _ => update() }
        value.remoteLeft foreach { _ => update() }
        update()

        mapping
      }

    @cutOutOfUserComputation lazy val asLocalFromAllSeq: Event[(Remote[R], T)] =
      value.cache(asLocalSeqId) {
        (asLocalFromAll map { remoteEvents =>
          (remoteEvents
            map { case (remote, event) => event map { (remote, _) } }
            reduceOption { _ || _ }
            getOrElse Evt())
        }).flatten
      }
  }

  implicit class RescalaEventOptionalAccessor[V, R, T, L](
    value: V from R)(implicit
    ev: Transmission[V, R, Event[T], L, Optional])
      extends RemoteAccessor {

    @cutOutOfUserComputation lazy val asLocal: Signal[Option[Event[T]]] =
      value.cache(asLocalId) {
        val option = transaction() { _ => Var(Option.empty[Event[T]]) }

        def update() = option.set(value.retrieveValue)

        value.remoteJoined foreach { _ => update() }
        value.remoteLeft foreach { _ => update() }
        update()

        option
      }

    @cutOutOfUserComputation lazy val asLocalSeq: Event[T] =
      value.cache(asLocalSeqId) { (asLocal map { _ getOrElse Evt() }).flatten }
  }

  implicit class RescalaEventSingleAccessor[V, R, T, L](
    value: V from R)(implicit
    ev: Transmission[V, R, Event[T], L, Single])
      extends RemoteAccessor {

    @cutOutOfUserComputation lazy val asLocal: Event[T] =
      value.retrieveValue
  }
}
