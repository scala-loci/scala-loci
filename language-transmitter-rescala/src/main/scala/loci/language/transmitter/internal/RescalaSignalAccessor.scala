package loci.language.transmitter.internal

import _root_.rescala.interface.RescalaInterface
import _root_.rescala.operator.cutOutOfUserComputation
import loci.language.Remote
import loci.language.transmitter.{Multiple, Optional, RemoteAccessor, Single, Transmission, from}

private[loci] trait RescalaSignalAccessor {
  val interface: RescalaInterface
  import interface._

  private final val asLocalId = 0

  implicit class RescalaSignalMultipleAccessor[V, R, T, L](
    value: V from R)(implicit
    ev: Transmission[V, R, Signal[T], L, Multiple])
      extends RemoteAccessor {

    @cutOutOfUserComputation lazy val asLocalFromAll: Signal[Seq[(Remote[R], Signal[T])]] =
      value.cache(asLocalId) {
        val mapping = transaction() { _ => Var(Seq.empty[(Remote[R], Signal[T])]) }

        def update() = mapping.set(value.remotes zip value.retrieveValues)

        value.remoteJoined foreach { _ => update() }
        value.remoteLeft foreach { _ => update() }
        update()

        mapping
      }
  }

  implicit class RescalaSignalOptionalAccessor[V, R, T, L](
    value: V from R)(implicit
    ev: Transmission[V, R, Signal[T], L, Optional])
      extends RemoteAccessor {

    @cutOutOfUserComputation lazy val asLocal: Signal[Option[T]] =
      value.cache(asLocalId) {
        val option = transaction() { _ => Var(Option.empty[Signal[T]]) }

        def update() = option.set(value.retrieveValue)

        value.remoteJoined foreach { _ => update() }
        value.remoteLeft foreach { _ => update() }
        update()

        option.flatten
      }
  }

  implicit class RescalaSignalSingleAccessor[V, R, T, L](
    value: V from R)(implicit
    ev: Transmission[V, R, Signal[T], L, Single])
      extends RemoteAccessor {

    @cutOutOfUserComputation lazy val asLocal: Signal[T] =
      value.retrieveValue
  }
}
