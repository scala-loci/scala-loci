package retier
package impl

import org.scalatest._
import util.Attributes

class MessageSpec extends FlatSpec with Matchers with OptionValues with TryValues {
  behavior of "Message"

  implicit class cleanOp(string: String) {
    def clean = string.lines map { _.stripMargin } mkString "\r\n"
  }

  it should "serialize messages correctly" in {
    (Message serialize Message(Message.Connect, Attributes("a" -> "0", "b" -> "1"), "")) should
      (be ("""Retier/Connect
             |a: 0
             |b: 1
             |""".clean) or
       be ("""Retier/Connect
             |b: 1
             |a: 0
             |""".clean))

    (Message serialize Message(Message.Connect, Attributes("a" -> "0", "a" -> "1"), "dummy")) should
      (be ("""Retier/Connect
             |a: 0
             |a: 1
             |
             |dummy""".clean) or
       be ("""Retier/Connect
             |a: 1
             |a: 0
             |
             |dummy""".clean))

    (Message serialize Message(Message.Content, Attributes.empty, "")) should
      be ("""Retier/Content
             |""".clean)

    (Message serialize Message(Message.Content, Attributes.empty, "dummy\r")) should
      be ("""Retier/Content
             |
             |dummy""".clean + "\r")

    (Message serialize Message(Message.Content, Attributes.empty, "dummy\n")) should
      be ("""Retier/Content
             |
             |dummy""".clean + "\n")
  }

  it should "deserialize messages correctly" in {
    val a = (Message deserialize "Retier/Connect").success.value

    a.method should be (Message.Connect)
    a.payload should be (empty)
    a.properties.toSeq should have size 0

    val b = (Message deserialize
      """Retier/Content
        | a: 0""".clean).success.value

    b.method should be (Message.Content)
    b.payload should be (empty)
    b.properties.toSeq should have size 1
    b.properties("a").value.values should be (Seq("0"))

    val c = (Message deserialize
      """Retier/Content
        | a: 0
        | b: 0
        |""".clean).success.value

    c.method should be (Message.Content)
    c.payload should be (empty)
    c.properties.toSeq should have size 2
    c.properties("a").value.values should be (Seq("0"))
    c.properties("b").value.values should be (Seq("0"))

    val d = (Message deserialize
      """Retier/Content
        | a: 0
        | a: 1
        | b: 0
        |
        |""".clean).success.value

    d.method should be (Message.Content)
    d.payload should be (empty)
    d.properties.toSeq should have size 3
    d.properties("a").value.values should be (Seq("0", "1"))
    d.properties("b").value.values should be (Seq("0"))

    val e = (Message deserialize
      """Retier/Content
        | a: 0
        | a: 1
        | b: 0
        | a: 2
        |
        |
        |""".clean).success.value

    e.method should be (Message.Content)
    e.payload should be ("\r\n")
    e.properties.toSeq should have size 4
    e.properties("a").value.values should be (Seq("0", "1", "2"))
    e.properties("b").value.values should be (Seq("0"))
  }

  it should "not deserialize messages incorrectly" in {
    (Message deserialize "Retier/Dummy").failure.exception should have message "Invalid message: invalid header"

    (Message deserialize
      """Retier/Content
        | : 0
        | b: 0
        |""".clean).failure.exception should have message "Invalid message: empty key"

    (Message deserialize
      """Retier/Content
        | a:
        | b: 0
        |""".clean).failure.exception should have message "Invalid message: empty value"

    (Message deserialize
      """Retier/Content
        | a
        | b: 0
        |""".clean).failure.exception should have message "Invalid message: missing value"
  }
}
