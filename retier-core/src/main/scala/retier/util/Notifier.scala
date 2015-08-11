package retier
package util

import java.util.concurrent.ConcurrentLinkedDeque
import scala.concurrent.ExecutionContext
import scala.collection.JavaConverters._

trait Notification[T] {
  private val listeners =
    new ConcurrentLinkedDeque[(T => Unit, Option[ExecutionContext])]

  protected def notify(v: T): Unit =
    listeners.asScala foreach {
      case (f, Some(ec)) => ec execute new Runnable { def run() = f(v) }
      case (f, None) => f(v)
    }

  def +=~>(f: T => Unit)(implicit ec: ExecutionContext): Unit =
    listeners add ((f, Some(ec)))

  def +=(f: T => Unit): Unit =
    listeners add ((f, None))

  def -=(f: T => Unit): Unit =
    listeners remove new {
      override def equals(other: Any) =
        other match { case (`f`, _) => true case _ => false }
    }
}

class Notifier[T] {
  protected class Notification extends util.Notification[T] {
    override protected[Notifier] def notify(v: T) = super.notify(v)
  }

  val notification = new Notification

  def apply(v: T): Unit = notification notify v
}

object Notifier {
  def apply[T] = new Notifier[T]
}
