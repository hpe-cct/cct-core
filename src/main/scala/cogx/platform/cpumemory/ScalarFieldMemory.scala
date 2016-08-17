/*
 * (c) Copyright 2016 Hewlett Packard Enterprise Development LP
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cogx.platform.cpumemory

import cogx.platform.types.FieldType
import cogx.platform.types.ElementTypes.Float32
import java.nio.FloatBuffer
import cogx.platform.opencl._
import cogx.platform.cpumemory.readerwriter.{ScalarFieldWriter, ScalarFieldReader, FieldReader}

/** CPU memory for a scalar field.
  *
  * @param fieldType Type of the field.
  * @param bufferType Type of Java Buffer used to hold data in the field.
  * @param commandQueue Command queue needed to create pinned buffers.
  *
  * @author Greg Snider
  */

final class ScalarFieldMemory private[cpumemory] (fieldType: FieldType,
                                            bufferType: BufferType,
                                            commandQueue: OpenCLParallelCommandQueue = null)
        extends AbstractFieldMemory(fieldType, bufferType, commandQueue)
        with ScalarFieldReader
        with ScalarFieldWriter
{
  require(tensorOrder == 0)
  require(elementType == Float32)
  if (bufferType == PinnedDirectBuffer)
    require(commandQueue != null)

  /** Byte buffer cast to appropriate buffer type to handle endianness. */
  _directBuffer = _byteBuffer.asFloatBuffer

  /** The direct buffer for this memory. This is always a view of _byteBuffer,
    * but upcast to the appropriate subclass of Buffer using, for example,
    * ByteBuffer.asFloatBuffer. The view calls are somewhat dangerous because
    * they create a view with proper endianness for Java (which uses the native
    * endianness internally) but they can also change the endianness of
    * _byteBuffer. This is subtle, so read the Java documentation carefully if
    * you need to change this.
    */
  def directBuffer: FloatBuffer = _directBuffer.asInstanceOf[FloatBuffer]

  /** Fill memory with values produced by an iterator. */
  def write(values: Iterator[Float]) {
    synchronized {
      dimensions match {
        case 0 => fill0D(values, directBuffer)
        case 1 => fill1D(values, directBuffer)
        case 2 => fill2D(values, directBuffer)
        case 3 => fill3D(values, directBuffer)
      }
    }
  }

  /** Fill memory with values from an Array[Float], Array[Array[Float]], etc. */
  def write(data: Array[_]) {
    synchronized {
      dimensions match {
        case 0 => fill0D(data.asInstanceOf[Array[Float]], directBuffer)
        case 1 => fill1D(data.asInstanceOf[Array[Float]], directBuffer)
        case 2 => fill2D(data.asInstanceOf[Array[Array[Float]]], directBuffer)
        case 3 => fill3D(data.asInstanceOf[Array[Array[Array[Float]]]], directBuffer)
      }
    }
  }

  /** Print out the field for debugging. */
  def print() {
    synchronized {
      dimensions match {
        case 0 => print0D()
        case 1 => print1D()
        case 2 => print2D()
        case 3 => print3D()
      }
    }
  }

  /** An iterator over all values in the field that assumes no column padding, scanning in row-major order. */
  def iterator =
    if (fieldHasColumnPadding)
      withPaddingIterator
    else
      noPaddingIterator

  private def fieldHasColumnPadding = columns != paddedColumns

  private def noPaddingIterator = new Iterator[Float] {
    private val points = fieldType.fieldShape.points
    private val buffer = directBuffer.duplicate
    buffer.rewind
    var i = 0
    def hasNext = i < points
    def next(): Float = {
      val value = buffer.get()
      i += 1
      value
    }
  }

  /** An iterator over all values in the field that handles column padding, scanning in row-major order. */
  private def withPaddingIterator = new Iterator[Float] {
    private var index = 0
    private var column = 0
    private val actualColumns = fieldType.columns
    private val columnPadding = paddedColumns - actualColumns
    private val lastIndex = layers * rows * paddedColumns - columnPadding
    private val buffer = directBuffer.duplicate
    buffer.rewind

    def hasNext = index < lastIndex
    def next(): Float = {
      val value = buffer.get(index)
      column += 1
      if (column == actualColumns) {
        column = 0
        index += columnPadding + 1
      } else
        index += 1
      value
    }
  }

  /** Read the single value in a 0D scalar field. */
  def read(): Float = {
    require(dimensions == 0)
    directBuffer.get(0)
  }

  /** Read the value at (`col`) in a 1D scalar field. */
  def read(col: Int): Float = {
    require(dimensions == 1)
    directBuffer.get(col)
  }

  /** Read the value at (`row`, `col`) in a 2D scalar field. */
  def read(row: Int, col: Int): Float = {
    require(dimensions == 2)
    directBuffer.get(row * paddedColumns + col)
  }

  /** Read the value at (`layer`, `row`, `col`) in a 3D scalar field. */
  def read(layer: Int, row: Int, col: Int): Float = {
    require(dimensions == 3)
    directBuffer.get(layer * rows * paddedColumns + row * paddedColumns + col)
  }

  /** Read the entire field as a flat array; used only for testing. */
  private[cogx] def readAsPaddedArray: Array[Float] = {
    val array = new Array[Float](bufferSize)
    for (i <- 0 until bufferSize)
      array(i) = directBuffer.get(i)
    array
  }

  /** Read the entire 0D or 1D field into the provided Array[Float]. */
  def get(dst: Array[Float]) {
    val buffer = directBuffer.duplicate
    buffer.rewind
    require(dst.size == bufferSize,
      s"Mismatched array size in ScalarFieldMemory.get(): expecting $bufferSize, saw ${dst.size}.")
    buffer.get(dst)
  }

  /** Read a portion of the values of the 0D or 1D scalar field into an
    * Array[Float], starting at the source buffer's `srcIndex` position.
    */
  def get(srcIndex: Int, dst: Array[Float]) {
    val buffer = directBuffer.duplicate
    buffer.position(srcIndex)
    require(srcIndex + dst.size <= bufferSize,
      s"ScalarFieldMemory.get() buffer overrun: ${dst.size} values requested, ${bufferSize - srcIndex} available.")
    buffer.get(dst)
  }

  /** Read `length` values of the 0D or 1D scalar field into the `dst`
    * Array[Float], starting at the source buffer's `srcIndex` position,
    * and placing the values in the `dst` Array starting at position
    * `dstIndex`.
    */
  def get(srcIndex: Int, dst: Array[Float], dstIndex: Int, length: Int) {
    val buffer = directBuffer.duplicate
    buffer.position(srcIndex)
    require(srcIndex + length <= bufferSize,
      s"ScalarFieldMemory.get() src buffer overrun: $length values requested, ${bufferSize - srcIndex} available.")
    require(dstIndex + length <= dst.length,
      s"ScalarFieldMemory.get() dst buffer overrun: $length locations targeted, ${dst.length - dstIndex} available.")
    buffer.get(dst, dstIndex, length)
  }

  /** Read the entire 2D field into the provided Array[Array[Float]] */
  def get(dst: Array[Array[Float]]) {
    require(dst.size * paddedColumns == bufferSize,
      s"Mismatched 2D array size in ScalarFieldMemory.get(): expecting (rows, columns) = "  +
        s"(${rows}, ${columns}), saw (${dst.size},${dst(0).size}).")
    val buffer = directBuffer.duplicate
    buffer.rewind
    if (fieldHasColumnPadding) {
      for (r <- 0 until dst.size) {
        buffer.position(r*paddedColumns)
        require(dst(r).size == columns,
          s"Mismatched column array in ScalarFieldMemory.get(), expecting $columns, saw ${dst(r).size}.")
        buffer.get(dst(r))
      }
    }
    else {
      for (r <- 0 until dst.size) {
        require(dst(r).size == columns,
          s"Mismatched column array in ScalarFieldMemory.get(), expecting $columns, saw ${dst(r).size}.")
        buffer.get(dst(r))
      }
    }
  }

  /** Read the entire 3D field into the provided Array[Array[Array[Float]]] */
  def get(dst: Array[Array[Array[Float]]]) {
    require(dst.size * dst(0).size * paddedColumns == bufferSize,
      s"Mismatched 3D array size in ScalarFieldMemory.get(): expecting (layers, rows, columns) = "  +
        s"(${layers}, ${rows}, ${columns}), saw (${dst.size},${dst(0).size},${dst(0)(0).size}).")
    val buffer = directBuffer.duplicate
    buffer.rewind
    if (fieldHasColumnPadding) {
      for (l <- 0 until dst.size) {
        for (r <- 0 until dst(l).size) {
          buffer.position(l*r*paddedColumns)
          require(dst(l)(r).size == columns,
            s"Mismatched column array in ScalarFieldMemory.get(), expecting $columns, saw ${dst(l)(r).size}.")
          buffer.get(dst(l)(r))
        }
      }
    }
    else {
      for (l <- 0 until dst.size) {
        for (r <- 0 until dst(l).size) {
          require(dst(l)(r).size == columns,
            s"Mismatched column array in ScalarFieldMemory.get(), expecting $columns, saw ${dst(l)(r).size}.")
          buffer.get(dst(l)(r))
        }
      }
    }
  }

  /** Read the entire field as a flat array without padding. */
  private[cogx] def readAsUnpaddedArray: Array[Float] = {
    // Create the unpadded array via a number of bulk gets (perhaps faster?).
    val bulkCopies = bufferSize / paddedColumns
    val points = bulkCopies * columns
    require(points == fieldType.fieldShape.points,
      "Internal error while unpacking padded FieldMemory")
    val array = new Array[Float](points)
    val buf = directBuffer.duplicate() // Makes this method thread-safe
    buf.rewind()
    for (i <- 0 until bulkCopies) {
      buf.position(i*paddedColumns)
      buf.get(array, i*columns, columns)
    }
    array
  }

  /** Write `value` to a 0D scalar field. */
  def write(value: Float) {
    require(dimensions == 0)
    directBuffer.put(0, value)
  }

  /** Write `value` at (`col`) in a 1D scalar field. */
  def write(col: Int, value: Float) {
    require(dimensions <= 1)
    directBuffer.put(col, value)
  }

  /** Write `value` at (`row`, `col`) in a 2D scalar field. */
  def write(row: Int, col: Int, value: Float) {
    require(dimensions == 2)
    directBuffer.put(row * paddedColumns + col, value)
  }

  /** Write `value` at (`layer`, `row`, `col`) in a 3D scalar field. */
  def write(layer: Int, row: Int, col: Int, value: Float ) {
    require(dimensions == 3)
    directBuffer.put(layer * rows * paddedColumns + row * paddedColumns + col, value)
  }


  /** Initialize the memory with a function. */
  private[cogx] def initialize(values: (_) => Float) {
    dimensions match {
      case 0 => init(values.asInstanceOf[(Int) => Float])
      case 1 => init(values.asInstanceOf[(Int) => Float])
      case 2 => init(values.asInstanceOf[(Int, Int) => Float])
      case 3 => init(values.asInstanceOf[(Int, Int, Int) => Float])
    }
  }

  /** Fill memory with values produced by `generator`. */
  private[cogx] def init(generator: () => Float) {
    require(dimensions == 0)
    write(generator())
  }

  /** Fill memory with values produced by `generator`. */
  private[cogx] def init(generator: (Int) => Float) {
    require(dimensions <= 1)
    for (col <- 0 until columns)
      write(col, generator(col))
  }

  /** Fill memory with values produced by `generator`. */
  private[cogx] def init(generator: (Int, Int) => Float) {
    require(dimensions == 2)
    for (row <- 0 until rows; col <- 0 until columns)
      write(row, col, generator(row, col))
  }

  /** Fill memory with values produced by `generator`. */
  private[cogx] def init(generator: (Int, Int, Int) => Float) {
    require(dimensions == 3)
    for (layer <- 0 until layers; row <- 0 until rows; col <- 0 until columns)
      write(layer, row, col, generator(layer, row, col))
  }

  /** Fill 0D memory with values produced by an iterator. */
  private def fill0D(values: Iterator[Float], to: FloatBuffer) {
    to.clear
    to.put(values.next())
    to.rewind
  }

  /** Fill 0D memory with values produced by an iterator. */
  private def fill0D(data: Array[Float], to: FloatBuffer) {
    to.clear
    to.put(data(0))
    to.rewind
  }

  /** Optimized helper function for filling N-D ScalarFieldMemories */
  private def fillColumn(values: Iterator[Float], to: FloatBuffer, columnLen: Int, paddedColumnLen: Int) {
    // To avoid the overhead of many put() calls, do the puts in chunks.
    val chunkSize = 64
    val a = new Array[Float](chunkSize)
    val chunks = columnLen/chunkSize
    for (chunk <- 0 until chunks) {
      for (i <- 0 until chunkSize)
        a(i) = values.next()
      // Put a whole chunk of values
      to.put(a)
    }
    // Output remaining values using single-float puts
    for (offset <- chunks * chunkSize until columnLen)
      to.put(values.next())
    // Fill padding with 0's
    for(col <- columnLen until paddedColumnLen)
      to.put(0f)
  }

  /** Optimized helper function for filling N-D ScalarFieldMemories */
  private def fillColumn(data: Array[Float], to: FloatBuffer, columnLen: Int, paddedColumnLen: Int) {
    to.put(data)
    // Fill padding with 0's
    for(col <- columnLen until paddedColumnLen)
      to.put(0f)
  }

  /** Fill 1D memory with values produced by an iterator. */
  private def fill1D(values: Iterator[Float], to: FloatBuffer) {
    to.clear
    fillColumn(values, to, columns, paddedColumns)
    to.rewind
  }

  /** Fill 1D memory with values produced by an iterator. */
  private def fill1D(data: Array[Float], to: FloatBuffer) {
    to.clear
    fillColumn(data, to, columns, paddedColumns)
    to.rewind
  }

  /** Fill 2D memory with values produced by an iterator. */
  private def fill2D(values: Iterator[Float], to: FloatBuffer) {
    to.clear
    if (fieldHasColumnPadding) {
      for (row <- 0 until rows) {
        fillColumn(values, to, columns, paddedColumns)
      }
    }
    else {
      val points = fieldType.fieldShape.points
      fillColumn(values, to, points, points)
    }
    to.rewind
  }

  /** Fill 2D memory with values produced by an iterator. */
  private def fill2D(data: Array[Array[Float]], to: FloatBuffer) {
    to.clear
    for (row <- 0 until rows)
      fillColumn(data(row), to, columns, paddedColumns)
    to.rewind
  }

  /** Fill 3D memory with values produced by an iterator. */
  private def fill3D(values: Iterator[Float], to: FloatBuffer) {
    to.clear
    if (fieldHasColumnPadding) {
      for (layer <- 0 until layers) {
        for (row <- 0 until rows) {
          fillColumn(values, to, columns, paddedColumns)
        }
      }
    }
    else {
      val points = fieldType.fieldShape.points
      fillColumn(values, to, points, points)
    }
    to.rewind
  }

  /** Fill 3D memory with values produced by an iterator. */
  private def fill3D(data: Array[Array[Array[Float]]], to: FloatBuffer) {
    to.clear
    for (layer <- 0 until layers) {
      for (row <- 0 until rows) {
        fillColumn(data(layer)(row), to, columns, paddedColumns)
      }
    }
    to.rewind
  }

  /** Compute the L-infinity norm on the difference of `this` and `that`.
    *
    * @param that AbstractFieldMemory to compare to `this`
    * @return L-infinity error
    */
  def compareLinf(that: FieldReader): Float = {
    require(fieldType.equals(that.fieldType))
    require(that.isInstanceOf[ScalarFieldReader])

    iterator.zip(that.asInstanceOf[ScalarFieldReader].iterator)
            .map(v => math.abs(v._1 - v._2))
            .max
  }

  /** Format of a single element in a field, for printing. */
  private val elementFormat = " %8.3f"

  /** Print out a 0D scalar field. */
  private def print0D() {
    printf(elementFormat, read())
    println()
  }

  /** Print out a 1D scalar field. */
  private def print1D() {
    for (col <- 0 until paddedColumns)
      printf(elementFormat, read(col))
    println()
  }

  /** Print out a 2D scalar field. */
  private def print2D() {
    for (row <- 0 until rows) {
      for (col <- 0 until paddedColumns) {
        printf(elementFormat, read(row, col))
      }
      println()
    }
    println()
  }

  /** Print out a 3D scalar field. */
  private def print3D() {
    for (layer <- 0 until layers) {
      for (row <- 0 until rows) {
        for (col <- 0 until paddedColumns) {
          printf(elementFormat, read(layer, row, col))
        }
        println()
      }
      println()
    }
    println()
  }
}
