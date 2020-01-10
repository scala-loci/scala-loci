package loci

import scala.annotation.compileTimeOnly
import scala.collection.mutable
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.scalajs.js.typedarray.Int8Array
import scala.scalajs.js.typedarray.TypedArrayBuffer
import scala.scalajs.js.typedarray.TypedArrayBufferOps._
import java.nio.CharBuffer
import java.nio.ByteBuffer
import java.nio.charset.CodingErrorAction
import java.nio.charset.StandardCharsets

final class MessageBuffer private (val backingArrayBuffer: ArrayBuffer)
    extends mutable.IndexedSeq[Byte] {
  @compileTimeOnly("`backingArray` only available on the JVM")
  def backingArray: Array[Byte] = ???

  @inline def length = backingArrayBuffer.byteLength

  @inline def apply(index: Int) = {
    if (index < 0  || index >= length)
      throw new IndexOutOfBoundsException(s"index $index")

    array(index)
  }

  @inline def update(index: Int, element: Byte) = {
    if (index < 0  || index >= length)
      throw new IndexOutOfBoundsException(s"index $index")

    array(index) = element
  }

  @inline def update(offset: Int, buffer: MessageBuffer, bufferOffset: Int, count: Int) = {
    if (offset < 0 || bufferOffset < 0 || count < 0 ||
      offset > length - count || bufferOffset > buffer.length - count)
      throw new IndexOutOfBoundsException(
        s"offset $offset, length $length, " +
        s"buffer offset ${bufferOffset}, buffer length ${buffer.length}, count $count")

    array set (new Int8Array(buffer.backingArrayBuffer, bufferOffset, count), offset)
  }

  @inline def concat(buffer: MessageBuffer): MessageBuffer = {
    val bufferArray = new Int8Array(length + buffer.length)
    bufferArray set (array, 0)
    bufferArray set (buffer.array, length)
    new MessageBuffer(bufferArray.buffer)
  }

  @inline def copy(offset: Int, count: Int): MessageBuffer = {
    if (offset < 0 || count < 0 || offset > length - count)
      throw new IndexOutOfBoundsException(s"offset $offset, count $count, length $length")

    new MessageBuffer(backingArrayBuffer slice (offset, offset + count))
  }

  @inline def toString(offset: Int, length: Int): String = {
    val decoder = StandardCharsets.UTF_8.newDecoder
      .onMalformedInput(CodingErrorAction.REPLACE)
      .onUnmappableCharacter(CodingErrorAction.REPLACE)

    val size = (length * decoder.maxCharsPerByte.toDouble).toInt
    val array = new Array[Char](size)
    val charBuffer = CharBuffer wrap array

    val byteBuffer = asByteBuffer
    byteBuffer position offset
    byteBuffer limit (offset + length)
    var result = decoder decode (byteBuffer, charBuffer, true)

    if (!result.isUnderflow)
      result.throwException
    result = decoder flush charBuffer
    if (!result.isUnderflow)
      result.throwException

    new String(array, 0, charBuffer.position())
  }

  @inline def asByteBuffer: ByteBuffer =
    TypedArrayBuffer wrap backingArrayBuffer

  private val array = new Int8Array(backingArrayBuffer)
}

object MessageBuffer {
  def empty: MessageBuffer = new MessageBuffer(new ArrayBuffer(0))

  def allocate(length: Int): MessageBuffer = new MessageBuffer(new ArrayBuffer(length))

  @compileTimeOnly("`wrapArray` only available on the JVM")
  def wrapArray(array: Array[Byte]): MessageBuffer = ???

  def fromString(string: String): MessageBuffer = {
    val array = string getBytes StandardCharsets.UTF_8
    val bufferArray = new Int8Array(array.length)
    var i = 0
    while (i < array.length) {
      bufferArray(i) = array(i)
      i += 1
    }
    new MessageBuffer(bufferArray.buffer)
  }

  def wrapByteBuffer(buffer: ByteBuffer): MessageBuffer =
    if (!buffer.hasArrayBuffer) {
      val duplicate = buffer.duplicate
      duplicate position 0
      duplicate limit buffer.capacity
      var pos = duplicate.remaining
      val bufferArray = new Int8Array(pos)
      while (pos > 0) {
        pos -= 1
        bufferArray(pos) = buffer get pos
      }
      new MessageBuffer(bufferArray.buffer)
    }
    else
      new MessageBuffer(buffer.arrayBuffer)

  def wrapArrayBuffer(arrayBuffer: ArrayBuffer): MessageBuffer =
    new MessageBuffer(arrayBuffer)
}
