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

import cogx.platform.types._
import cogx.platform.types.ElementTypes.Complex32
import cogx.cogmath.algebra.complex.Complex
import java.nio.FloatBuffer
import cogx.platform.opencl.OpenCLParallelCommandQueue
import cogx.platform.cpumemory.readerwriter.{ComplexFieldWriter, ComplexFieldReader, FieldReader}
import cogx.cogmath.geometry.Shape

/** CPU memory for a complex scalar field.
  *
  * @param fieldType Type of the field.
  * @param bufferType Type of Java Buffer used to hold data in the field.
  * @param commandQueue Command queue needed to create pinned buffers.
  *
  * @author Greg Snider
  */

final class ComplexFieldMemory private[cpumemory] (fieldType: FieldType,
                                             bufferType: BufferType,
                                             commandQueue: OpenCLParallelCommandQueue = null)
        extends AbstractFieldMemory(fieldType, bufferType)
        with ComplexFieldReader
        with ComplexFieldWriter
{
  require(tensorOrder == 0)
  require(elementType == Complex32)
  private val writable = true
  if (bufferType == PinnedDirectBuffer)
    require(commandQueue != null)

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
  def read(): Complex = {
    require(dimensions == 0)
    readComplex(0)
  }

  /** Read the value at (`col`) in a 1D vector field into `out`. */
  def read(col: Int): Complex = {
    require(dimensions == 1)
    readComplex(col)
  }

  /** Read the value at (`row`, `col`) in a 2D vector field into `out`. */
  def read(row: Int, col: Int): Complex = {
    require(dimensions == 2)
    readComplex(row * paddedColumns + col)
  }

  /** Read the value at (`layer`, `row`, `col`) in a 3D vector field into `out`. */
  def read(layer: Int, row: Int, col: Int): Complex = {
    require(dimensions == 3)
    readComplex(layer * rows * paddedColumns + row * paddedColumns + col)
  }

  /** Read the entire field as a flat array; used only for testing. */
  private[cogx] def readAsPaddedArray: Array[Float] = {
    val array = new Array[Float](bufferSize)
    for (i <- 0 until bufferSize)
      array(i) = directBuffer.get(i)
    array
  }

  /** Write `out` to a 0D vector field. */
  def write(out: Complex) {
    require(dimensions == 0 && writable)
    writeComplex(0, out)
  }

  /** Write `out` to a 1D vector field at (`col`). */
  def write(col: Int, out: Complex) {
    require(dimensions == 1 && writable)
    writeComplex(col, out)
  }

  /** Write `out` to a 2D vector field at (`row`, `col`). */
  def write(row: Int, col: Int, out: Complex) {
    require(dimensions == 2 && writable)
    writeComplex(row * paddedColumns + col, out)
  }

  /** Write `out` to a 3D vector field at (`row`, `col`). */
  def write(layer: Int, row: Int, col: Int, out: Complex) {
    require(dimensions == 3 && writable)
    writeComplex(layer * rows * paddedColumns + row * paddedColumns + col, out)
  }

  /** Read a complex scalar from the field memory into an array.
    *
    * @param startIndex Index in direct buffer for the start of the tensor.
    */
  private def readComplex(startIndex: Int): Complex = {
    var index = startIndex
    val real = directBuffer.asInstanceOf[FloatBuffer].get(index)
    val imaginary = directBuffer.asInstanceOf[FloatBuffer].get(index + partStride)
    new Complex(real, imaginary)
  }

  /** Write a complex scalar from an array into the field memory.
    *
    * @param startIndex Index in direct buffer for the start of the tensor.
    * @param from Array where tensor is to be read
    */
  private def writeComplex(startIndex: Int, from: Complex) {
    var index = startIndex
    directBuffer.asInstanceOf[FloatBuffer].put(index, from.real)
    directBuffer.asInstanceOf[FloatBuffer].put(index + partStride, from.imaginary)
  }

  /** Fill memory with values produced by `generator`. */
  private[cogx] def init(generator: () => Complex) {
    require(dimensions == 0)
    write(generator())
  }

  /** Fill memory with values produced by `generator`. */
  private[cogx] def init(generator: (Int) => Complex) {
    require(dimensions == 1)
    for (col <- 0 until fieldType.fieldShape(0))
      write(col, generator(col))
  }

  /** Fill memory with values produced by `generator`. */
  private[cogx] def init(generator: (Int, Int) => Complex) {
    require(dimensions == 2)
    val rows = fieldType.fieldShape(0)
    val columns = fieldType.fieldShape(1)
    for (row <- 0 until rows; col <- 0 until columns)
      write(row, col, generator(row, col))
  }

  /** Fill memory with values produced by `generator`. */
  private[cogx] def init(generator: (Int, Int, Int) => Complex) {
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
    require(fieldType.equals(that.fieldType))
    require(that.isInstanceOf[ComplexFieldReader])
    require(dimensions >= 0 && dimensions <= 3)

    dimensions match {
      case 0 => {
        val c1 = read()
        val c2 = that.asInstanceOf[ComplexFieldReader].read()
        (c1 - c2).magnitude
      }

      case 1 => {
        var err = 0f

        for (i <- 0 until fieldShape(0)) {
          val c1 = read(i)
          val c2 = that.asInstanceOf[ComplexFieldReader].read(i)
          err = err.max((c1 - c2).magnitude)
        }

        err
      }

      case 2 => {
        var err = 0f

        for (i <- 0 until fieldShape(0)) {
          for (j <- 0 until fieldShape(1)) {
            val c1 = read(i, j)
            val c2 = that.asInstanceOf[ComplexFieldReader].read(i, j)
            err = err.max((c1 - c2).magnitude)
          }
        }

        err
      }

      case 3 => {
        var err = 0f

        for (i <- 0 until fieldShape(0)) {
          for (j <- 0 until fieldShape(1)) {
            for (k <- 0 until fieldShape(2)) {
              val c1 = read(i, j, k)
              val c2 = that.asInstanceOf[ComplexFieldReader].read(i, j, k)
              err = err.max((c1 - c2).magnitude)
            }
          }
        }

        err
      }
    }
  }

  /** Print out the field for debugging. */
  def print {
    synchronized {
      dimensions match {
        case 0 => print0D
        case 1 => print1D
        case 2 => print2D
        case 3 => print3D
      }
    }
  }

  /** Print out a 0D scalar field. */
  private def print0D {
    Predef.print(read)
    println
  }

  /** Print out a 1D scalar field. */
  private def print1D {
    for (col <- 0 until paddedColumns)
      Predef.print(read(col))
    println
  }

  /** Print out a 2D scalar field. */
  private def print2D {
    for (row <- 0 until rows) {
      for (col <- 0 until paddedColumns) {
        Predef.print(read(row, col))
      }
      println
    }
    println
  }

  /** Print out a 3D scalar field. */
  private def print3D {
    for (layer <- 0 until layers) {
      for (row <- 0 until rows) {
        for (col <- 0 until paddedColumns) {
          Predef.print(read(layer, row, col))
        }
        println
      }
      println
    }
    println
  }
}

/** Factory for creating ComplexFieldMemories.
  *
  * THIS IS ONLY USED FOR TESTING. NOT EXPORTED TO USERS.
  */
object ComplexFieldMemory {

  /** Create a stand-alone 0D ComplexFieldMemory for testing.
    *
    * @param complex The single complex to put in the field.
    * @return Initialized complex field memory.
    */
  def apply(complex: Complex): ComplexFieldMemory = {
    val fieldType =
      new FieldType(Shape(), Shape(), Complex32)
    val bufferType = IndirectBuffer
    val memory = new ComplexFieldMemory(fieldType, bufferType)
    memory.write(complex)
    memory
  }

  /** Create a stand-alone 1D ComplexFieldMemory for testing.
    *
    * @param columns Columns in field.
    * @param f Function which supplies a value for every point in the field.
    * @return Initialized complex field memory.
    */
  def apply(columns: Int, f:(Int) => Complex): ComplexFieldMemory = {
    val fieldType =
      new FieldType(Shape(columns), Shape(), Complex32)
    val bufferType = IndirectBuffer
    val memory = new ComplexFieldMemory(fieldType, bufferType)
    for (r <- 0 until columns)
      memory.write(r, f(r))
    memory
  }

  /** Create a stand-alone 2D ComplexFieldMemory for testing.
    *
    * @param rows Rows in field.
    * @param columns Columns in field.
    * @param f Function which supplies a value for every point in the field.
    * @return Initialized complex field memory.
    */
  def apply(rows: Int, columns: Int, f:(Int, Int) => Complex): ComplexFieldMemory = {
    val fieldType =
      new FieldType(Shape(rows, columns), Shape(), Complex32)
    val bufferType = IndirectBuffer
    val memory = new ComplexFieldMemory(fieldType, bufferType)
    for (r <- 0 until rows; c <- 0 until columns)
      memory.write(r, c, f(r, c))
    memory
  }

  /** Create a stand-alone 3D ComplexFieldMemory for testing.
    *
    * @param layers Layers in field.
    * @param rows Rows in field.
    * @param columns Columns in field.
    * @param f Function which supplies a value for every point in the field.
    * @return Initialized complex field memory.
    */
  def apply(layers: Int, rows: Int, columns: Int, f:(Int, Int, Int) => Complex): ComplexFieldMemory = {
    val fieldType =
      new FieldType(Shape(layers, rows, columns), Shape(), Complex32)
    val bufferType = IndirectBuffer
    val memory = new ComplexFieldMemory(fieldType, bufferType)
    for (l <- 0 until layers; r <- 0 until rows; c <- 0 until columns)
      memory.write(l, r, c, f(l, r, c))
    memory
  }

}