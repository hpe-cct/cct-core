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
import cogx.platform.types.ElementTypes.Complex32
import java.nio.FloatBuffer
import cogx.platform.opencl.OpenCLParallelCommandQueue
import cogx.cogmath.algebra.complex.{ComplexArray, ComplexVector}
import cogx.platform.cpumemory.readerwriter._

/** CPU memory for a complex vector field.
  *
  * @param fieldType Type of the field.
  * @param bufferType Type of Java Buffer used to hold data in the field.
  * @param commandQueue Command queue needed to create pinned buffers.
  *
  * @author Greg Snider
  */

final class ComplexVectorFieldMemory private[cogx] (fieldType: FieldType,
                                              bufferType: BufferType,
                                              commandQueue: OpenCLParallelCommandQueue = null)
        extends AbstractFieldMemory(fieldType, bufferType)
        with ComplexVectorFieldReader
        with ComplexVectorFieldWriter
{
  require(tensorOrder == 1)
  require(elementType == Complex32)
  if (bufferType == PinnedDirectBuffer)
    require(commandQueue != null)
  private val writable = true

  /** Low level byte buffer for this field, required by base class. */
  _byteBuffer = {
    bufferType match {
      case PinnedDirectBuffer =>
        allocatePinnedDirectByteBuffer(bufferSizeBytes, commandQueue)
      case DirectBuffer =>
        allocateDirectByteBuffer(bufferSizeBytes)
      case IndirectBuffer =>
        allocateIndirectByteBuffer(bufferSizeBytes)
    }
  }
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

  /** Read the single value in a 0D vector field into `out`. */
  def read(out: ComplexVector) {
    require(dimensions == 0)
    readComplexTensor(0, out.asComplexArray)
  }

  /** Read the value at (`col`) in a 1D vector field into `out`. */
  def read(col: Int, out: ComplexVector) {
    require(dimensions == 1)
    readComplexTensor(col, out.asComplexArray)
  }

  /** Read the value at (`row`, `col`) in a 2D vector field into `out`. */
  def read(row: Int, col: Int, out: ComplexVector) {
    require(dimensions == 2)
    readComplexTensor(row * paddedColumns + col, out.asComplexArray)
  }

  /** Read the value at (`layer`, `row`, `col`) in a 3D vector field into `out`. */
  def read(layer: Int, row: Int, col: Int, out: ComplexVector) {
    require(dimensions == 3)
    readComplexTensor(layer * rows * paddedColumns + row * paddedColumns + col, out.asComplexArray)
  }

  /** Read the entire field as a flat array; used only for testing. */
  private[cogx] def readAsPaddedArray: Array[Float] = {
    val array = new Array[Float](bufferSize)
    for (i <- 0 until bufferSize)
      array(i) = directBuffer.get(i)
    array
  }

  /** Write `out` to a 0D vector field. */
  def write(out: ComplexVector) {
    require(dimensions == 0 && writable)
    writeComplexTensor(0, out.asComplexArray)
  }

  /** Write `out` to a 1D vector field at (`col`). */
  def write(col: Int, out: ComplexVector) {
    require(dimensions == 1 && writable)
    writeComplexTensor(col, out.asComplexArray)
  }

  /** Write `out` to a 2D vector field at (`row`, `col`). */
  def write(row: Int, col: Int, out: ComplexVector) {
    require(dimensions == 2 && writable)
    writeComplexTensor(row * paddedColumns + col, out.asComplexArray)
  }

  /** Write `out` to a 3D vector field at (`row`, `col`). */
  def write(layer: Int, row: Int, col: Int, out: ComplexVector) {
    require(dimensions == 3 && writable)
    writeComplexTensor(layer * rows * paddedColumns + row * paddedColumns + col,
      out.asComplexArray)
  }

  /** Read a complex tensor from the field memory into an array.
     *
     * @param startIndex Index in direct buffer for the start of the tensor.
     * @param to Array where tensor is to be written.
     */
  private def readComplexTensor(startIndex: Int, to: ComplexArray) {
    var index = startIndex
    for (i <- 0 until tensorShape.points) {
      val real = directBuffer.get(index)
      val imaginary = directBuffer.get(index + partStride)
      to.update(i, real, imaginary)
      index += tensorStride
    }
  }

  /** Write a complex tensor from an array into the field memory.
    *
    * @param startIndex Index in direct buffer for the start of the tensor.
    * @param from Array where tensor is to be read
    */
  private def writeComplexTensor(startIndex: Int, from: ComplexArray) {
    var index = startIndex
    for (i <- 0 until tensorShape.points) {
      val real = from.real(i)
      val imaginary = from.imaginary(i)
      directBuffer.asInstanceOf[FloatBuffer].put(index, real)
      directBuffer.asInstanceOf[FloatBuffer].put(index + partStride, imaginary)
      index += tensorStride
    }
  }

  /** Fill memory with values produced by `generator`. */
  private[cogx] def init(generator: () => ComplexVector) {
    require(dimensions == 0)
    write(generator())
  }

  /** Fill memory with values produced by `generator`. */
  private[cogx] def init(generator: (Int) => ComplexVector) {
    require(dimensions == 1)
    for (col <- 0 until fieldType.fieldShape(0))
      write(col, generator(col))
  }

  /** Fill memory with values produced by `generator`. */
  private[cogx] def init(generator: (Int, Int) => ComplexVector) {
    require(dimensions == 2)
    val rows = fieldType.fieldShape(0)
    val columns = fieldType.fieldShape(1)
    for (row <- 0 until rows; col <- 0 until columns)
      write(row, col, generator(row, col))
  }

  /** Fill memory with values produced by `generator`. */
  private[cogx] def init(generator: (Int, Int, Int) => ComplexVector) {
    require(dimensions == 3)
    val layers = fieldType.fieldShape(0)
    val rows = fieldType.fieldShape(1)
    val columns = fieldType.fieldShape(2)
    for (layer <- 0 until layers; row <- 0 until rows; col <- 0 until columns)
      write(layer, row, col, generator(layer, row, col))
  }

  /** Compute the L-infinity norm on the difference of `this` and `that`.
    *
    * @param that AbstractFieldMemory to compare to `this`
    * @return L-infinity error
    */
  def compareLinf(that: FieldReader): Float = {
    /*
    require(fieldType.equals(that.fieldType))
    require(that.isInstanceOf[VectorFieldReader])
    require(dimensions >= 0 && dimensions <= 3)
    require(tensorShape.dimensions == 1)

    val v1 = ComplexVector(tensorShape(0), _ => 0f)
    val v2 = Vector(tensorShape(0), _ => 0f)

    dimensions match {
      case 0 => {
        val v1 = read()
        that.asInstanceOf[VectorFieldReader].read(v2)
        (v1 - v2).abs.reduce(_.max(_))
      }

      case 1 => {
        var err = 0f

        for (i <- 0 until fieldShape(0)) {
          read(i, v1)
          that.asInstanceOf[VectorFieldReader].read(i, v2)
          err = err.max((v1 - v2).abs.reduce(_.max(_)))
        }

        err
      }

      case 2 => {
        var err = 0f

        for (i <- 0 until fieldShape(0)) {
          for (j <- 0 until fieldShape(1)) {
            read(i, j, v1)
            that.asInstanceOf[VectorFieldReader].read(i, j, v2)
            err = err.max((v1 - v2).abs.reduce(_.max(_)))
          }
        }

        err
      }

      case 3 => {
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
    */
    0f
  }

  /** Print out the field for debugging. */
  def print() {
    throw new RuntimeException("not implemented yet")
  }

}
