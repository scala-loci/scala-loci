package loci
package runtime

import scala.collection.mutable

object Value {
  case class Signature(name: String, path: List[String])

  object Signature {
    def serialize(signature: Signature): String =
      if (signature.path.isEmpty)
        signature.name
      else
        s"${signature.path mkString "."}.${signature.name}"

    def deserialize(signature: String): Signature = {
      var first = 0
      var last = 0
      val end = signature.length
      val buffer = mutable.ListBuffer.empty[String]

      while (last < end)
        signature(last) match {
          case '.' =>
            buffer += signature.substring(first, last)
            last += 1
            first = last
          case '(' | ':' =>
            last = end
          case _ =>
            last += 1
        }

      Signature(signature.substring(first, end), buffer.toList)
    }
  }

  case class Reference(link: String, remote: Remote.Reference, system: System)
      extends transmitter.AbstractionRef {
    def channel: Channel = system.obtainChannel(link, remote)
    def derive(name: String) = Reference(link + ":" + name, remote, system)
  }
}
