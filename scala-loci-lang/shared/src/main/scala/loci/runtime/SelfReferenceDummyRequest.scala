package loci
package runtime

import scala.concurrent.Future

sealed trait SelfReferenceDummyRequest[R] {
  this: transmitter.Transmission[_, R, _, _, _] =>

  /**
   * caching is unnecessary for local values
   */
  override private[loci] def cache[B <: AnyRef](id: Any, body: => B): B = body

  override private[loci] val remoteJoined: Notice.Stream[Remote[R]] = {
    Notice.Stream[Remote[R]].notice
  }

  override private[loci] val remoteLeft: Notice.Stream[Remote[R]] = {
    Notice.Stream[Remote[R]].notice
  }

  override private[loci] def remotesReferences: Seq[Remote[R]] = {
    throw new NotImplementedError(s"remoteReferences should not be called on ${getClass.getSimpleName}")
  }
}

/**
 * Wrap value into a Future to streamline with what [[RemoteRequest]] returns (only use if [[T]] is itself not a Future)
 */
final class FutureWrappingSelfReferenceDummyRequest[V, R, T, L, M](
  value: T
) extends transmitter.Transmission[V, R, Future[T], L, M] with SelfReferenceDummyRequest[R] {
  override private[loci] def retrieveValues: Seq[Future[T]] = {
    Seq(Future.successful(value))
  }
}

/**
 * Return value as is if it is already a Future, in order to avoid unnecessarily nested Futures, which is in line with
 * [[RemoteRequest]]
 */
final class IdenticalSelfReferenceDummyRequest[V, R, T <: Future[_], L, M](
  value: T
) extends transmitter.Transmission[V, R, T, L, M] with SelfReferenceDummyRequest[R] {
  override private[loci] def retrieveValues: Seq[T] = {
    Seq(value)
  }
}
