package loci
package runtime

import scala.collection.mutable

object Value {
  case class Signature(name: String, module: String, path: List[String]) {
    override def toString: String =
      if (path.isEmpty) s"$module.$name" else s"${path mkString "."}.$name[$module]"
  }

  object Signature {
    def serialize(signature: Signature): String =
      if (signature.path.isEmpty)
        s"${signature.module}!${signature.name}"
      else
        s"${signature.module}!${signature.path mkString "."}.${signature.name}"

    def deserialize(signature: String): Signature = {
      var first = 0
      var last = 0
      val end = signature.length
      val buffer = mutable.ListBuffer.empty[String]

      while (last < end && first < end)
        signature(last) match {
          case '!' =>
            first = end
          case _ =>
            last += 1
        }

      val module = signature.substring(0, last)
      if (last < end)
        last += 1
      first = last

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

      Signature(signature.substring(first, end), module, buffer.toList)
    }
  }

  case class Reference(channelName: String, channelAnchor: String,
        remote: Remote.Reference, system: System)
      extends transmitter.AbstractionRef {
    lazy val channel: Channel = system.obtainChannel(channelName, channelAnchor, remote)
    def derive(name: String) = Reference(s"$channelName:$name", channelAnchor, remote, system)

    override def toString: String = s"[channel:$channelName]$remote"
  }
}
