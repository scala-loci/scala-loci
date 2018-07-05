package loci
package communicator

import scala.util.Try
import scala.concurrent.duration.Duration

trait ConnectionSetupParser {
  type Properties

  protected abstract class PropertyParser[T](
    val parse: List[String] => Option[T])

  protected abstract class PropertyParserTry[T](parse: List[String] => T)
    extends PropertyParser[T](value => Try { parse(value) }.toOption)

  protected implicit class PropertiesParsingOp(properties: Properties) {
    def set[T: PropertyParser]
        (key: String)(transform: T => Properties => Properties)
        (implicit props: ConnectionSetupFactory.Properties) =
      (props get key
        flatMap { implicitly[PropertyParser[T]] parse _ }
        map { transform(_)(properties) }
        getOrElse properties)
  }
}

trait SimpleConnectionSetupProperties { this: ConnectionSetupParser =>
  protected implicit object booleanParser
    extends PropertyParserTry(_.head.toBoolean)
  protected implicit object byteParser
    extends PropertyParserTry(_.head.toByte)
  protected implicit object shortParser
    extends PropertyParserTry(_.head.toShort)
  protected implicit object intParser
    extends PropertyParserTry(_.head.toInt)
  protected implicit object longParser
    extends PropertyParserTry(_.head.toLong)
  protected implicit object floatParser
    extends PropertyParserTry(_.head.toFloat)
  protected implicit object doubleParser
    extends PropertyParserTry(_.head.toDouble)
  protected implicit object stringParser
    extends PropertyParserTry(_.head)
  protected implicit object durationParser
    extends PropertyParserTry(value => Duration(value.head))
  protected implicit object finiteDurationParser
    extends PropertyParserTry(value =>
      Duration fromNanos Duration(value.head).toNanos)
  protected implicit object byteListParser
    extends PropertyParserTry(_ map { _.toByte })
  protected implicit object shortListParser
    extends PropertyParserTry(_ map { _.toShort })
  protected implicit object intListParser
    extends PropertyParserTry(_ map { _.toInt })
  protected implicit object longListParser
    extends PropertyParserTry(_ map { _.toLong })
  protected implicit object floatListParser
    extends PropertyParserTry(_ map { _.toFloat })
  protected implicit object doubleListParser
    extends PropertyParserTry(_ map { _.toDouble })
  protected implicit object stringListParser
    extends PropertyParserTry(identity)
  protected implicit object durationListParser
    extends PropertyParserTry(_ map { Duration(_) })
  protected implicit object finiteDurationListParser
    extends PropertyParserTry(_ map { Duration fromNanos Duration(_).toNanos })
}
