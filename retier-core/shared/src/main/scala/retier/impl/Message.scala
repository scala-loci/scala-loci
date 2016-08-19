package retier
package impl

import util.Attributes
import util.Value
import scala.collection.mutable.ListBuffer
import scala.util.Try

case class Message(
  method: Message.Method, properties: Attributes, payload: String)

object AcceptMessage {
  def apply(): Message =
    Message(Message.Connect, Attributes("Type" -> "Accept"), "")
  def unapply(msg: Message): Boolean =
    (msg.method, msg.properties("Type")) match {
      case (Message.Connect, Value("Accept")) => true
      case _ => false
    }
}

object RequestMessage {
  def apply(requested: String, requesting: String): Message =
    Message(Message.Connect, Attributes(
      "Type" -> "Request",
      "Requested" -> requested,
      "Requesting" -> requesting), "")
  def unapply(msg: Message): Option[(String, String)] =
    (msg.method,
     msg.properties("Type"),
     msg.properties("Requested"),
     msg.properties("Requesting")) match {
      case (Message.Connect,
        Value("Request"),
        Value(requested),
        Value(requesting)) => Some((requested, requesting))
      case _ => None
    }
}

object StartedMessage {
  def apply(): Message =
    Message(Message.Content, Attributes("Type" -> "Started"), "")
  def unapply(msg: Message): Boolean =
    (msg.method, msg.properties("Type")) match {
      case (Message.Content, Value("Started")) => true
      case _ => false
    }
}

object ChannelMessage {
  def apply(messageType: String, channel: String, abstraction: Option[String],
      payload: String): Message = {
    val attrs = Seq("Type" -> messageType, "Channel" -> channel)
    val attrsAbstraction = (abstraction map { abstraction =>
      "Abstraction" -> abstraction
    }).toSeq
    Message(Message.Content, Attributes(attrs ++ attrsAbstraction), payload)
  }
  def unapply(msg: Message): Option[(String, String, Option[String], String)] =
    (msg.method,
     msg.properties("Type"),
     msg.properties("Channel"),
     msg.properties("Abstraction")) match {
      case (Message.Content,
          Value(messageType),
          Value(channel),
          Value(abstraction)) =>
        Some((messageType, channel, Some(abstraction), msg.payload))
      case (Message.Content,
          Value(messageType),
          Value(channel), _) =>
        Some((messageType, channel, None, msg.payload))
      case _ => None
    }
}

class MessageException(msg: String) extends IllegalArgumentException(msg)

object Message {
  sealed abstract class Method
  final case object Connect extends Method
  final case object Content extends Method

  def serialize(msg: Message): String = {
    val builder = new StringBuilder

    msg.method match {
      case Connect => builder ++= "Retier/Connect\r\n"
      case Content => builder ++= "Retier/Content\r\n"
    }

    msg.properties.toSeq foreach { case (key, value) =>
      builder ++= s"$key: $value\r\n"
    }

    if (msg.payload.nonEmpty)
      builder ++= s"\r\n${msg.payload}"

    builder.toString
  }

  def deserialize(str: String): Try[Message] = Try {
    val (header, properties, payload) = parse(str)
    Message(
      header match {
        case "Retier/Connect" => Connect
        case "Retier/Content" => Content
        case _ => throwInvalidHeader
      },
      properties,
      payload)
  }

  private def throwNoHeader =
    throw new MessageException("Invalid message: no header")
  private def throwInvalidHeader =
    throw new MessageException("Invalid message: invalid header")
  private def throwEmptyKey =
    throw new MessageException("Invalid message: empty key")
  private def throwEmptyValue =
    throw new MessageException("Invalid message: empty value")
  private def throwMissingValue =
    throw new MessageException("Invalid message: missing value")

  private def parse(str: String): (String, Attributes, String) = {
    val stateHeader = 0
    val stateBeforeKey = 1
    val stateKey = 2
    val stateValue = 3
    val stateBeforePayload = 4
    val statePayload = 5

    var state = stateHeader
    var header = ""
    var key = ""
    var payload = ""
    val properties = ListBuffer.empty[(String, String)]
    val builder = new StringBuilder(str.length)

    def makeHeader() = {
      header = builder.toString.trim
      builder.clear
      if (header.isEmpty)
        throwNoHeader
    }

    def makeKey() = {
      key = builder.toString.trim
      builder.clear
      if (key.isEmpty)
        throwEmptyKey
    }

    def makeProperty() = {
      val value = builder.toString.trim
      properties += key -> value
      key = ""
      builder.clear
      if (value.isEmpty)
        throwEmptyValue
    }

    def makePayload() = {
      payload = builder.toString
    }

    for (ch <- str)
      state match {
        case `stateHeader` =>
          ch match {
            case '\r' | '\n' =>
              makeHeader
              state = if (ch == '\r') stateBeforeKey else stateKey
            case _  =>
              builder += ch
          }

        case `stateBeforeKey` =>
          ch match {
            case '\r' => state = stateBeforePayload
            case '\n' => state = stateKey
            case _  => state = stateKey; builder += ch
          }

        case `stateKey` =>
          ch match {
            case '\r' | '\n' =>
              if (builder.toString.trim.nonEmpty)
                throwMissingValue
              state = if (ch == '\r') stateBeforePayload else statePayload
            case ':' =>
              makeKey
              state = stateValue
            case _  =>
              builder += ch
          }

        case `stateValue` =>
          ch match {
            case '\r' | '\n' =>
              makeProperty
              state = if (ch == '\r') stateBeforeKey else stateKey
            case _  =>
              builder += ch
          }

        case `stateBeforePayload` =>
          ch match {
            case '\n' => state = statePayload
            case _  => state = statePayload; builder += ch
          }

        case `statePayload` =>
          builder += ch
      }

    state match {
      case `stateHeader` => makeHeader
      case `stateKey` => if (builder.toString.trim.nonEmpty) throwMissingValue
      case `stateValue` => makeProperty
      case `statePayload` => makePayload
    }

    (header, Attributes(properties), payload)
  }
}
