package retier
package util

import java.util.concurrent.ConcurrentLinkedDeque
import scala.concurrent.ExecutionContext
import scala.collection.JavaConverters._

trait Notification[T] { self =>
  private val listeners =
    new ConcurrentLinkedDeque[(T => _, Option[ExecutionContext])]

  protected def notify(v: T): Unit =
    listeners.asScala foreach {
      case (f, Some(ec)) => ec execute new Runnable { def run() = f(v) }
      case (f, None) => f(v)
    }

  private def createTransformedNotification[U]
      (transformation: PartialFunction[T, U],
       oec: Option[ExecutionContext]): Notification[U] =
    new Notification[U] {

      private[this] val propagate: T => Unit = value =>
        if (transformation isDefinedAt value)
          notify(transformation(value))

      override def +=~>[R](f: U => R)(implicit ec: ExecutionContext): Unit =
        listeners synchronized {
          if (listeners.isEmpty) {
            if (oec.isEmpty)
              self.+=(propagate)
            else
              self.+=~>(propagate)(oec.get)
          }
          super.+=~>(f)(ec)
        }

      override def +=[R](f: U => R): Unit = listeners synchronized {
        if (listeners.isEmpty) {
          if (oec.isEmpty)
            self.+=(propagate)
          else
            self.+=~>(propagate)(oec.get)
        }
        super.+=(f)
      }

      override def -=[R](f: U => R): Unit = listeners synchronized {
        super.-=(f)
        if (listeners.isEmpty)
          self -= propagate
      }
    }

  def transformInContext[U](transformation: PartialFunction[T, U])
      (implicit ec: ExecutionContext): Notification[U] =
    createTransformedNotification(transformation, Some(ec))

  def transform[U](transformation: PartialFunction[T, U]): Notification[U] =
    createTransformedNotification(transformation, None)

  def inContext(implicit ec: ExecutionContext): Notification[T] =
    createTransformedNotification(PartialFunction(identity), Some(ec))

  def +=~>[R](f: T => R)(implicit ec: ExecutionContext): Unit =
    listeners add ((f, Some(ec)))

  def +=[R](f: T => R): Unit =
    listeners add ((f, None))

  def -=[R](f: T => R): Unit =
    listeners remove new {
      override def equals(other: Any) =
        other match { case (`f`, _) => true case _ => false }
    }
}

class Notifier[T] {
  protected class NotifierNotification extends Notification[T] {
    override protected[Notifier] def notify(v: T) = super.notify(v)
  }

  protected val notifierNotification = new NotifierNotification

  val notification: Notification[T] = notifierNotification

  def apply(v: T): Unit =
    notifierNotification notify v
  def apply()(implicit ev: Unit =:= T): Unit =
    notifierNotification notify ev(())
}

object Notifier {
  def apply[T] = new Notifier[T]
}
