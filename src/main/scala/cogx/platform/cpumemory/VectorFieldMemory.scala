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

import cogx.cogmath.algebra.real.Vector
import cogx.platform.types.FieldType
import cogx.platform.types.ElementTypes.Float32
import java.nio.FloatBuffer
import cogx.platform.opencl.OpenCLParallelCommandQueue
import cogx.platform.cpumemory.readerwriter.{VectorFieldWriter, VectorFieldReader, FieldReader}
import cogx.cogmath.geometry.Shape

/** CPU memory for a vector field.
  *
  * @param fieldType Type of the field.
  * @param bufferType Type of Java Buffer used to hold data in the field.
  * @param commandQueue Command queue needed to create pinned buffers.
  *
  * @author Greg Snider
  */

final class VectorFieldMemory private[cpumemory] (fieldType: FieldType,
                                            bufferType: BufferType,
                                            commandQueue: OpenCLParallelCommandQueue = null)
        extends AbstractFieldMemory(fieldType, bufferType, commandQueue)
        with VectorFieldReader
        with VectorFieldWriter
{
  require(tensorOrder == 1)
  require(elementType == Float32)
  if (bufferType == PinnedDirectBuffer)
    require(commandQueue != null)
  private val writable = true

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
  def write(values: Iterator[Vector]) {
    synchronized {
      dimensions match {
        case 0 => fill0D(values)
        case 1 => fill1D(values)
        case 2 => fill2D(values)
        case 3 => fill3D(values)
      }
    }
  }

  /** Read the single value in a 0D vector field into `out`. */
  def read(out: Vector) {
    require(dimensions == 0)
    readTensor(0, out.asArray)
  }

  /** Read the value at (`col`) in a 1D vector field into `out`. */
  def read(col: Int, out: Vector) {
    require(dimensions == 1)
    readTensor(col, out.asArray)
  }

  /** Read the value at (`row`, `col`) in a 2D vector field into `out`. */
  def read(row: Int, col: Int, out: Vector) {
    require(dimensions == 2)
    readTensor(row * paddedColumns + col, out.asArray)
  }

  /** Read the value at (`layer`, `row`, `col`) in a 3D vector field into `out`. */
  def read(layer: Int, row: Int, col: Int, out: Vector) {
    require(dimensions == 3)
    readTensor(layer * rows * paddedColumns + row * paddedColumns + col, out.asArray)
  }

  /** Read the entire field as a flat array; used only for testing. */
  private[cogx] def readAsPaddedArray: Array[Float] = {
    val array = new Array[Float](bufferSize)
    for (i <- 0 until bufferSize)
      array(i) = directBuffer.get(i)
    array
  }

  /** Read a tensor from the field memory into an array.
    *
    * @param startIndex Index in direct buffer for the start of the tensor.
    * @param to Array where tensor is to be written.
    */
  private def readTensor(startIndex: Int, to: Array[Float]) {
    var index = startIndex
    for (i <- 0 until tensorShape.points) {
      to(i) = directBuffer.get(index)
      index += tensorStride
    }
  }

  /** Write a tensor from an array into the field memory.
    *
    * @param startIndex Index in direct buffer for the start of the tensor.
    * @param from Array where tensor is to be read
    */
  private def writeTensor(startIndex: Int, from: Array[Float]) {
    var index = startIndex
    for (i <- 0 until tensorShape.points) {
      directBuffer.put(index, from(i))
      index += tensorStride
    }
  }

  /** Write `out` to a 0D vector field. */
  def write(out: Vector) {
    require(dimensions == 0 && writable)
    writeTensor(0, out.asArray)
  }

  /** Write `out` to a 1D vector field at (`col`). */
  def write(col: Int, out: Vector) {
    require(dimensions == 1 && writable)
    writeTensor(col, out.asArray)
  }

  /** Write `out` to a 2D vector field at (`row`, `col`). */
  def write(row: Int, col: Int, out: Vector) {
    require(dimensions == 2 && writable)
    writeTensor(row * paddedColumns + col, out.asArray)
  }

  /** Write `out` to a 3D vector field at (`row`, `col`). */
  def write(layer: Int, row: Int, col: Int, out: Vector) {
    require(dimensions == 3 && writable)
    writeTensor(layer * rows * paddedColumns + row * paddedColumns + col, out.asArray)
  }

  /** Fill memory with values produced by `generator`. */
  private[cogx] def init(generator: () => Vector) {
    require(dimensions == 0)
    write(generator())
  }

  /** Fill memory with values produced by `generator`. */
  private[cogx] def init(generator: (Int) => Vector) {
    require(dimensions == 1)
    for (col <- 0 until fieldType.fieldShape(0))
      write(col, generator(col))
  }

  /** Fill memory with values produced by `generator`. */
  private[cogx] def init(generator: (Int, Int) => Vector) {
    require(dimensions == 2)
    val rows = fieldType.fieldShape(0)
    val columns = fieldType.fieldShape(1)
    for (row <- 0 until rows; col <- 0 until columns)
      write(row, col, generator(row, col))
  }

  /** Fill memory with values produced by `generator`. */
  private[cogx] def init(generator: (Int, Int, Int) => Vector) {
    require(dimensions == 3)
    val layers = fieldType.fieldShape(0)
    val rows = fieldType.fieldShape(1)
    val columns = fieldType.fieldShape(2)
    for (layer <- 0 until layers; row <- 0 until rows; col <- 0 until columns)
      write(layer, row, col, generator(layer, row, col))
  }

  /** Fill memory with values produced by `generator`. */
  private[cogx] def fill0D(values: Iterator[Vector]) {
    require(dimensions == 0 && writable)
    writeTensor(0, values.next().asArray)
  }

  /** Fill memory with values produced by `generator`. */
  private[cogx] def fill1D(values: Iterator[Vector]) {
    require(dimensions == 1 && writable)
    for (col <- 0 until fieldType.fieldShape(0))
      writeTensor(col, values.next().asArray)
  }

  /** Fill memory with values produced by `generator`. */
  private[cogx] def fill2D(values: Iterator[Vector]) {
    require(dimensions == 2 && writable)
    val rows = fieldType.fieldShape(0)
    val columns = fieldType.fieldShape(1)
    for (row <- 0 until rows; col <- 0 until columns)
      writeTensor(row * paddedColumns + col,
        values.next().asArray)
  }

  /** Fill memory with values produced by `generator`. */
  private[cogx] def fill3D(values: Iterator[Vector]) {
    require(dimensions == 3 && writable)
    val layers = fieldType.fieldShape(0)
    val rows = fieldType.fieldShape(1)
    val columns = fieldType.fieldShape(2)
    for (layer <- 0 until layers; row <- 0 until rows; col <- 0 until columns)
      writeTensor(layer * rows * paddedColumns + row * paddedColumns + col,
        values.next().asArray)
  }

  /** Compute the L-infinity norm on the difference of `this` and `that`.
    *
    * @param that AbstractFieldMemory to compare to `this`
    * @return L-infinity error
    */
  def compareLinf(that: FieldReader): Float = {
    require(fieldType.equals(that.fieldType))
    require(that.isInstanceOf[VectorFieldReader])
    require(dimensions >= 0 && dimensions <= 3)
    require(tensorShape.dimensions == 1)

    val v1 = Vector(tensorShape(0), _ => 0f)
    val v2 = Vector(tensorShape(0), _ => 0f)

    dimensions match {
      case 0 =>
        read(v1)
        that.asInstanceOf[VectorFieldReader].read(v2)
        (v1 - v2).abs.reduce(_.max(_))

      case 1 =>
        var err = 0f
        for (i <- 0 until fieldShape(0)) {
          read(i, v1)
          that.asInstanceOf[VectorFieldReader].read(i, v2)
          err = err.max((v1 - v2).abs.reduce(_.max(_)))
        }
        err

      case 2 =>
        var err = 0f
        for (i <- 0 until fieldShape(0)) {
          for (j <- 0 until fieldShape(1)) {
            read(i, j, v1)
            that.asInstanceOf[VectorFieldReader].read(i, j, v2)
            err = err.max((v1 - v2).abs.reduce(_.max(_)))
          }
        }
        err

      case 3 =>
        var err = 0f
        for (i <- 0 until fieldShape(0)) {
          for (j <- 0 until fieldShape(1)) {
            for (k <- 0 until fieldShape(2)) {
              read(i, j, k, v1)
              that.asInstanceOf[VectorFieldReader].read(i, j, k, v2)
              err = err.max((v1 - v2).abs.reduce(_.max(_)))
            }
          }
        }
        err

    }
  }

  /** Print out the field for debugging. */
  def print() {
    throw new RuntimeException("not implemented yet")
  }

}
