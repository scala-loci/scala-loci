package loci
package runtime

import messaging.Message

import java.util.UUID

sealed abstract class Method
case object Connect extends Method
case object Content extends Method

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
          Some(Seq(requesting))) =>
        Some((requested, requesting))
      case _ =>
        None
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

object CloseMessage {
  def apply(channel: String): Message[Method] =
    Message(
      Content,
      Map("Type" -> Seq("Close"), "Channel" -> Seq(channel)),
      MessageBuffer.empty)

  def unapply(message: Message[Method]): Option[String] =
    (message.method, message.properties get "Channel") match {
      case (Content, Some(Seq(channel))) =>
        Some(channel)
      case _ =>
        None
    }
}

object ChannelMessage {
  type Type = Type.Value

  object Type extends Enumeration {
    val Request = Value("Request")
    val Call = Value("Call")
    val Response = Value("Response")
    val Failure = Value("Failure")
    val Update = Value("Update")
  }

  def apply(messageType: Type, channel: String, abstraction: Option[String],
      payload: MessageBuffer): Message[Method] = {
    val attrs = Map("Type" -> Seq(messageType.toString), "Channel" -> Seq(channel))
    val attrsAbstraction = (abstraction map { "Abstraction" -> Seq(_) }).toSeq
    Message(Content, attrs ++ attrsAbstraction, payload)
  }

  def unapply(message: Message[Method]): Option[(Type, String, Option[String], MessageBuffer)] =
    (message.method,
     message.properties get "Type" map {
       _ map { messageType => Type.values find { _.toString == messageType } }
     },
     message.properties get "Channel",
     message.properties get "Abstraction") match {
      case (Content,
          Some(Seq(Some(messageType))),
          Some(Seq(channel)),
          Some(Seq(abstraction))) =>
        Some((messageType, channel, Some(abstraction), message.payload))
      case (Content,
          Some(Seq(Some(messageType))),
          Some(Seq(channel)),
          None) =>
        Some((messageType, channel, None, message.payload))
      case _ =>
        None
    }
}

object PeerIdExchangeMessage {

  def apply(peerId: UUID, init: Boolean): Message[Method] = {
    val exchangeType = if (init) "PeerIdExchangeInit" else "PeerIdExchangeResponse"
    Message(
      Content,
      Map("Type" -> Seq(exchangeType)),
      MessageBuffer.encodeString(peerId.toString)
    )
  }

  def unapply(message: Message[Method]): Option[(UUID, Boolean)] = {
    (message.method, message.properties get "Type") match {
      case (Content, Some(Seq("PeerIdExchangeInit"))) => Some((UUID.fromString(message.payload.decodeString), true))
      case (Content, Some(Seq("PeerIdExchangeResponse"))) => Some((UUID.fromString(message.payload.decodeString), false))
      case _ => None
    }
  }
}

object NetworkMonitoringMessage {

  def apply(time: Long, isResponse: Boolean): Message[Method] = {
    val messageType = if (isResponse) "PingResponse" else "PingRequest"
    Message(Content, Map("Type" -> Seq(messageType)), MessageBuffer.encodeString(time.toString))
  }

  def unapply(message: Message[Method]): Option[(Long, Boolean)] = {
    (message.method, message.properties get "Type") match {
      case (Content, Some(Seq("PingRequest"))) => Some((message.payload.decodeString.toLong, false))
      case (Content, Some(Seq("PingResponse"))) => Some((message.payload.decodeString.toLong, true))
      case _ => None
    }
  }
}
