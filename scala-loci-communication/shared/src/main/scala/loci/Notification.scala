package loci

import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal
import java.util.concurrent.ConcurrentLinkedQueue

trait Notification[+T] {
  def notify[U >: T](notifiable: U => Unit): Notifiable[U]
}

trait Notifiable[-T] extends Function1[T, Unit] {
  def remove(): Unit
}

class Notifier[T](failureReporter: Throwable => Unit) {
  private[this] val notifierNotification =
    new Notifier.NotifierNotification[T](failureReporter)

  val notification: Notification[T] = notifierNotification

  def apply(v: T): Unit =
    notifierNotification notify v
  def apply()(implicit ev: Unit =:= T): Unit =
    notifierNotification notify ev(())
}

object Notifier {
  private class NotifierNotification[T](failureReporter: Throwable => Unit)
      extends Notification[T] {
    private[this] val notifiables = new ConcurrentLinkedQueue[T => Unit]

    def notify(v: T): Unit = {
      val iterator = notifiables.iterator
      while (iterator.hasNext)
        try iterator.next()(v)
        catch { case NonFatal(exception) => failureReporter(exception) }
    }

    def notify[U >: T](notifiable: U => Unit): Notifiable[U] = {
      notifiables add notifiable
      new Notifiable[U] {
        def apply(v: U) = notifiable(v)
        def remove() = notifiables remove notifiable
      }
    }
  }

  def apply[T] =
    new Notifier[T](ExecutionContext.defaultReporter)
  def apply[T](failureReporter: Throwable => Unit) =
    new Notifier[T](failureReporter)
}
