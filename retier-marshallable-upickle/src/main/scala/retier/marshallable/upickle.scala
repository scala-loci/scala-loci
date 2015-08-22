package retier
package marshallable

import transmission.Transmittable
import transmission.PullBasedTransmittable
import transmission.PushBasedTransmittable
import transmission.TransmittableMarshalling._
import scala.util.Try

trait LowPriorityUpickleImplicits { this: upickle.type =>
  implicit def upickleBasedDefaultPullBasedMarshallable[T]
      (implicit reader: Reader[T], writer: Writer[T]) =
    defaultTransmittable createMarshallable (writer.marshall, reader.unmarshall)
}

object upickle
    extends LowPriorityUpickleImplicits
    with _root_.upickle.AttributeTagged {
  def tagName = "$type"

  implicit def upickleBasedPullBasedMarshallable[T, U]
    (implicit
        transmittable: PullBasedTransmittable[T, U],
        reader: Reader[U], writer: Writer[U]) =
    transmittable createMarshallable (writer.marshall, reader.unmarshall)

  implicit def upickleBasedPushBasedMarshallable[T, U]
    (implicit
        transmittable: PushBasedTransmittable[T, U],
        reader: Reader[U], writer: Writer[U]) =
    transmittable createMarshallable (writer.marshall, reader.unmarshall)

  implicit def transmittableBasedReader[T, U]
      (implicit transmittable: Transmittable[T, U], reader: Reader[U]) =
    Reader[T] { case expr =>
      transmittable receive (reader read expr, reader.unmarshall)
    }

  implicit def transmittableBasedWriter[T, U]
      (implicit transmittable: Transmittable[T, U], writer: Writer[U]) =
    Writer[T] { case expr =>
      writer write (transmittable send (expr, writer.marshall))
    }

  protected implicit class WriterMarshalling[T](writer: Writer[T]) {
    def marshall = { v: T => write(v)(writer) }
  }

  protected implicit class ReaderMarshalling[T](reader: Reader[T]) {
    def unmarshall = { v: String => Try { read(v)(reader) } }
  }
}
