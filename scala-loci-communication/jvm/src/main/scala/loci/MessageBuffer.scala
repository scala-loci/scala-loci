package loci

import scala.annotation.compileTimeOnly
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

final class MessageBuffer private (val backingArray: Array[Byte])
    extends IndexedSeq[Byte] {
  @compileTimeOnly("`backingArrayBuffer` only available in JS")
  def backingArrayBuffer: Any = ???

  @inline def length: Int = backingArray.length

  @inline def apply(index: Int): Byte = backingArray(index)

  @inline def concat(buffer: MessageBuffer): MessageBuffer = {
    val array = new Array[Byte](length + buffer.length)
    System arraycopy (backingArray, 0, array, 0, length)
    System arraycopy (buffer.backingArray, 0, array, length, buffer.length)
    new MessageBuffer(array)
  }

  @inline def copy(offset: Int, length: Int): MessageBuffer = {
    val array = new Array[Byte](length)
    System arraycopy (backingArray, offset, array, 0, length)
    new MessageBuffer(array)
  }

  @inline def toString(offset: Int, length: Int): String =
    new String(backingArray, offset, length, StandardCharsets.UTF_8)

  @inline def asByteBuffer: ByteBuffer =
    ByteBuffer wrap backingArray
}

object MessageBuffer {
  def empty: MessageBuffer = new MessageBuffer(Array.emptyByteArray)

  def fromString(string: String): MessageBuffer =
    new MessageBuffer(string getBytes StandardCharsets.UTF_8)

  def wrapByteBuffer(buffer: ByteBuffer): MessageBuffer =
    if (!buffer.hasArray) {
      val duplicate = buffer.duplicate
      duplicate position 0
      duplicate limit buffer.capacity
      val array = new Array[Byte](duplicate.remaining)
      duplicate get array
      new MessageBuffer(array)
    }
    else
      new MessageBuffer(buffer.array)

  def wrapArray(array: Array[Byte]): MessageBuffer = new MessageBuffer(array)

  @compileTimeOnly("`wrapArrayBuffer` only available in JS")
  def wrapArrayBuffer(arrayBuffer: Any): MessageBuffer = ???
}
