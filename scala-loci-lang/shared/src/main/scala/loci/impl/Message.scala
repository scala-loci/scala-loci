package loci
package impl

import messaging.Message

sealed abstract class Method
final case object Connect extends Method
final case object Content extends Method

object Method {
  implicit val method = messaging.Message.Method[Method](
    Connect -> "Loci/Connect",
    Content -> "Loci/Content")
}

object AcceptMessage {
  def apply(): Message[Method] =
    Message(Connect, Map("Type" -> Seq("Accept")), MessageBuffer.empty)

  def unapply(message: Message[Method]): Boolean =
    (message.method, message.properties get "Type") match {
      case (Connect, Some(Seq("Accept"))) => true
      case _ => false
    }
}

object RequestMessage {
  def apply(requested: String, requesting: String): Message[Method] =
    Message(
      Connect,
      Map(
        "Type" -> Seq("Request"),
        "Requested" -> Seq(requested),
        "Requesting" -> Seq(requesting)),
      MessageBuffer.empty)

  def unapply(message: Message[Method]): Option[(String, String)] =
    (message.method,
     message.properties get "Type",
     message.properties get "Requested",
     message.properties get "Requesting") match {
      case (Connect,
        Some(Seq("Request")),
        Some(Seq(requested)),
        Some(Seq(requesting))) => Some((requested, requesting))
      case _ => None
    }
}

object StartedMessage {
  def apply(): Message[Method] =
    Message(Content, Map("Type" -> Seq("Started")), MessageBuffer.empty)

  def unapply(message: Message[Method]): Boolean =
    (message.method, message.properties get "Type") match {
      case (Content, Some(Seq("Started"))) => true
      case _ => false
    }
}

object ChannelMessage {
  def apply(messageType: String, channel: String, abstraction: Option[String],
      payload: MessageBuffer): Message[Method] = {
    val attrs = Map("Type" -> Seq(messageType), "Channel" -> Seq(channel))
    val attrsAbstraction = (abstraction map {
      "Abstraction" -> Seq(_)
    }).toSeq
    Message(Content, attrs ++ attrsAbstraction, payload)
  }

  def unapply(message: Message[Method]): Option[(String, String, Option[String], MessageBuffer)] =
    (message.method,
     message.properties get "Type",
     message.properties get "Channel",
     message.properties get "Abstraction") match {
      case (Content,
          Some(Seq(messageType)),
          Some(Seq(channel)),
          Some(Seq(abstraction))) =>
        Some((messageType, channel, Some(abstraction), message.payload))
      case (Content,
          Some(Seq(messageType)),
          Some(Seq(channel)),
          _) =>
        Some((messageType, channel, None, message.payload))
      case _ => None
    }
}
