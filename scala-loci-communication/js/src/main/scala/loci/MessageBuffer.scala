package loci

import scala.annotation.compileTimeOnly
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.scalajs.js.typedarray.Int8Array
import scala.scalajs.js.typedarray.TypedArrayBuffer
import scala.scalajs.js.typedarray.TypedArrayBufferOps._
import java.nio.CharBuffer
import java.nio.ByteBuffer
import java.nio.charset.CodingErrorAction
import java.nio.charset.StandardCharsets

final class MessageBuffer private (val backingArrayBuffer: ArrayBuffer)
    extends IndexedSeq[Byte] {
  @compileTimeOnly("`backingArray` only available on the JVM")
  def backingArray: Seq[Byte] = ???

  @inline def length = backingArrayBuffer.byteLength

  @inline def apply(index: Int) = array(index)

  @inline def concat(buffer: MessageBuffer): MessageBuffer = {
    val bufferArray = new Int8Array(length + buffer.length)
    bufferArray set (array, 0)
    bufferArray set (buffer.array, length)
    new MessageBuffer(bufferArray.buffer)
  }

  @inline def copy(offset: Int, length: Int): MessageBuffer =
    new MessageBuffer(backingArrayBuffer slice (offset, offset + length))

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
