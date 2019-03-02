package loci.dev
package runtime

import scala.collection.mutable

final class SignatureParser private (content: String, start: Int, end: Int) {
  private def parse(start: Int, end: Int): List[SignatureParser] = {
    var depth = 0
    var first = start + 1
    var last = start + 1
    val buffer = mutable.ListBuffer.empty[SignatureParser]

    if (start >= end - 1)
      throw new IllegalArgumentException("Malformed signature: "+
        s"Expected '[...]' but found empty string")

    if (content(start) != '[' || content(end - 1) != ']')
      throw new IllegalArgumentException("Malformed signature: "+
        s"Expected '[...]' but found '${content.substring(start, end)}'")

    if (last < end - 1) {
      while (last < end - 1) {
        if (content(last) == '[')
          depth += 1
        else if (content(last) == ']') {
          depth -= 1
          if (depth < 0)
            throw new IllegalArgumentException("Malformed signature: "+
              s"Superfluous ']' in ...${content.substring(start + 1, last + 1)}...")
        }
        else if (content(last) == ',' && depth == 0) {
          buffer += new SignatureParser(content, first, last)
          first = last + 1
        }
        last += 1
      }

      if (depth != 0)
        throw new IllegalArgumentException("Malformed signature: "+
          s"Missing ']' in '${content.substring(start, end)}'")

      buffer += new SignatureParser(content, first, last)
    }

    buffer.toList
  }

  def asElements(count: Int): List[SignatureParser] = {
    val elements = parse(start, end)
    if (elements.size != count)
      throw new IllegalArgumentException("Malformed signature: "+
        s"Expected $count elements but found ${elements.size} elements in '${content.substring(start, end)}'")

    elements
  }

  def asList: List[SignatureParser] =
    parse(start, end)

  def asString: String =
    content.substring(start, end)

  override def toString = asString
}

object SignatureParser {
  def apply(content: String) = new SignatureParser(content, 0, content.length)

  def elements(elements: String*) = elements.mkString("[", ",", "]")
  def list(list: List[String]) = list.mkString("[", ",", "]")
  def string(string: String) = string
}
