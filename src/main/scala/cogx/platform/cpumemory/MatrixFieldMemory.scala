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

import cogx.cogmath.algebra.real.Matrix
import cogx.platform.types.FieldType
import cogx.platform.types.ElementTypes.Float32
import java.nio.FloatBuffer
import cogx.platform.opencl.OpenCLParallelCommandQueue
import cogx.platform.cpumemory.readerwriter.{MatrixFieldReader, FieldReader, MatrixFieldWriter}
import cogx.cogmath.geometry.Shape

/** CPU memory for a matrix field.
  *
  * @param fieldType Type of the field.
  * @param bufferType Type of Java Buffer used to hold data in the field.
  * @param commandQueue Command queue needed to create pinned buffers.
  *
  * @author Greg Snider
  */

final class MatrixFieldMemory private[cpumemory] (fieldType: FieldType,
                                            bufferType: BufferType,
                                            commandQueue: OpenCLParallelCommandQueue = null)
        extends AbstractFieldMemory(fieldType, bufferType)
        with MatrixFieldReader
        with MatrixFieldWriter
{
  require(tensorOrder == 2)
  require(elementType == Float32)
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

  /** Read the single value in a 0D matrix field into `out`. */
  def read(out: Matrix) {
    require(dimensions == 0)
    readTensor(0, out.asArray)
  }

  /** Read the value at (`col`) in a 1D matrix field into `out`. */
  def read(col: Int, out: Matrix) {
    require(dimensions == 1)
    readTensor(col, out.asArray)
  }

  /** Read the value at (`row`, `col`) in a 2D matrix field into `out`. */
  def read(row: Int, col: Int, out: Matrix) {
    require(dimensions == 2)
    readTensor(row * paddedColumns + col, out.asArray)
  }

  /** Read the value at (`layer`, `row`, `col`) in a 3D matrix field into `out`. */
  def read(layer: Int, row: Int, col: Int, out: Matrix) {
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

  /** Write `out` to a 0D matrix field. */
  def write(out: Matrix) {
    require(dimensions == 0)
    writeTensor(0, out.asArray)
  }

  /** Write `out` to a 1D matrix field at (`col`). */
  def write(col: Int, out: Matrix) {
    require(dimensions == 1)
    writeTensor(col, out.asArray)
  }

  /** Write `out` to a 2D matrix field at (`row`, `col`). */
  def write(row: Int, col: Int, out: Matrix) {
    require(dimensions == 2)
    writeTensor(row * paddedColumns + col, out.asArray)
  }

  /** Write `out` to a 3D matrix field at (`row`, `col`). */
  def write(layer: Int, row: Int, col: Int, out: Matrix) {
    require(dimensions == 3)
    writeTensor(layer * rows * paddedColumns + row * paddedColumns + col, out.asArray)
  }

  /** Fill memory with values produced by `generator`. */
  private[cogx] def init(generator: () => Matrix) {
    require(dimensions == 0)
    write(generator())
  }

  /** Fill memory with values produced by `generator`. */
  private[cogx] def init(generator: (Int) => Matrix) {
    require(dimensions == 1)
    for (col <- 0 until fieldType.fieldShape(0))
      write(col, generator(col))
  }

  /** Fill memory with values produced by `generator`. */
  private[cogx] def init(generator: (Int, Int) => Matrix) {
    require(dimensions == 2)
    val rows = fieldType.fieldShape(0)
    val columns = fieldType.fieldShape(1)
    for (row <- 0 until rows; col <- 0 until columns)
      write(row, col, generator(row, col))
  }

  /** Fill memory with values produced by `generator`. */
  private[cogx] def init(generator: (Int, Int, Int) => Matrix) {
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
    require(that.isInstanceOf[MatrixFieldReader])
    require(dimensions >= 0 && dimensions <= 3)
    require(tensorShape.dimensions == 2)

    val m1 = Matrix(tensorShape(0), tensorShape(1), (_, _) => 0f)
    val m2 = Matrix(tensorShape(0), tensorShape(1), (_, _) => 0f)

    dimensions match {
      case 0 =>
        read(m1)
        that.asInstanceOf[MatrixFieldReader].read(m2)
        (m1 - m2).abs.reduce(_.max(_))

      case 1 =>
        var err = 0f
        for (i <- 0 until fieldShape(0)) {
          read(i, m1)
          that.asInstanceOf[MatrixFieldReader].read(i, m2)
          err = err.max((m1 - m2).abs.reduce(_.max(_)))
        }
        err

      case 2 =>
        var err = 0f
        for (i <- 0 until fieldShape(0)) {
          for (j <- 0 until fieldShape(1)) {
            read(i, j, m1)
            that.asInstanceOf[MatrixFieldReader].read(i, j, m2)
            err = err.max((m1 - m2).abs.reduce(_.max(_)))
          }
        }
        err

      case 3 =>
        var err = 0f
        for (i <- 0 until fieldShape(0)) {
          for (j <- 0 until fieldShape(1)) {
            for (k <- 0 until fieldShape(2)) {
              read(i, j, k, m1)
              that.asInstanceOf[MatrixFieldReader].read(i, j, k, m2)
              err = err.max((m1 - m2).abs.reduce(_.max(_)))
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


/** Factory for creating MatrixFieldMemories.
  *
  * THIS IS ONLY USED FOR TESTING. NOT EXPORTED TO USERS.
  */
object MatrixFieldMemory {

  /** Create a stand-alone 0D MatrixFieldMemory for testing.
    *
    * @param matrix The single matrix in the field.
    * @return Initialized matrix field memory.
    */
  def apply(matrix: Matrix): MatrixFieldMemory = {
    val fieldType =
      new FieldType(Shape(), matrix.shape, Float32)
    val bufferType = IndirectBuffer
    val memory = new MatrixFieldMemory(fieldType, bufferType)
    memory.write(matrix)
    memory
  }

  /** Create a stand-alone 1D MatrixFieldMemory for testing.
    *
    * @param columns Columns in field.
    * @param f Function which supplies a value for every point in the field.
    * @return Initialized matrix field memory.
    */
  def apply(columns: Int, f:(Int) => Matrix): MatrixFieldMemory = {
    val fieldType =
      new FieldType(Shape(columns), f(0).shape, Float32)
    val bufferType = IndirectBuffer
    val memory = new MatrixFieldMemory(fieldType, bufferType)
    for (r <- 0 until columns)
      memory.write(r, f(r))
    memory
  }

  /** Create a stand-alone 2D MatrixFieldMemory for testing.
    *
    * @param rows Rows in field.
    * @param columns Columns in field.
    * @param f Function which supplies a value for every point in the field.
    * @return Initialized matrix field memory.
    */
  def apply(rows: Int, columns: Int, f:(Int, Int) => Matrix): MatrixFieldMemory = {
    val fieldType =
      new FieldType(Shape(rows, columns), f(0, 0).shape, Float32)
    val bufferType = IndirectBuffer
    val memory = new MatrixFieldMemory(fieldType, bufferType)
    for (r <- 0 until rows; c <- 0 until columns)
      memory.write(r, c, f(r, c))
    memory
  }

  /** Create a stand-alone 3D MatrixFieldMemory for testing.
    *
    * @param layers Layers in field.
    * @param rows Rows in field.
    * @param columns Columns in field.
    * @param f Function which supplies a value for every point in the field.
    * @return Initialized matrix field memory.
    */
  def apply(layers: Int, rows: Int, columns: Int, f:(Int, Int, Int) => Matrix): MatrixFieldMemory = {
    val fieldType =
      new FieldType(Shape(layers, rows, columns), f(0, 0, 0).shape, Float32)
    val bufferType = IndirectBuffer
    val memory = new MatrixFieldMemory(fieldType, bufferType)
    for (l <- 0 until layers; r <- 0 until rows; c <- 0 until columns)
      memory.write(l, r, c, f(l, r, c))
    memory
  }
}