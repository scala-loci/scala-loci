package loci
package runtime

import scala.concurrent.Future

final class SelfReferenceDummyRequest[V, R, T, L, M](
  value: T
) extends transmitter.Transmission[V, R, Future[T], L, M] {

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

  /**
   * Just wrap the value into a future to streamline the type with what [[RemoteRequest]] returns
   */
  override private[loci] def retrieveValues: Seq[Future[T]] = {
    Seq(Future.successful(value))
  }
}
