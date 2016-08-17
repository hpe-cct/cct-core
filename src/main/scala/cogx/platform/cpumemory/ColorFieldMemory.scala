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

import cogx.platform.types.{Pixel, FieldType}
import cogx.platform.types.ElementTypes.Uint8Pixel
import java.nio.ByteBuffer
import cogx.platform.opencl.OpenCLParallelCommandQueue
import cogx.platform.cpumemory.readerwriter.{ColorFieldWriter, ColorFieldReader, FieldReader}
import cogx.cogmath.geometry.Shape

/** CPU memory for an image.
  *
  * Internally this assumes RGBA format, which is the only format guaranteed
  * to be supported by OpenCL.
  *
  * @param fieldType Type of the field.
  * @param bufferType Type of Java Buffer used to hold data in the field.
  * @param commandQueue Command queue needed to create pinned buffers.
  *
  * @author Greg Snider
  */

final class ColorFieldMemory private[cpumemory] (fieldType: FieldType,
                                           bufferType: BufferType,
                                           commandQueue: OpenCLParallelCommandQueue = null)
        extends AbstractFieldMemory(fieldType, bufferType, commandQueue)
        with ColorFieldReader
        with ColorFieldWriter
{
  require(tensorOrder == 1)
  require(elementType == Uint8Pixel)
  if (bufferType == PinnedDirectBuffer)
    require(commandQueue != null)

  /** Byte buffer cast to appropriate buffer type to handle endianness. */
  _directBuffer = _byteBuffer

  /** The direct buffer for this memory. This is always a view of _byteBuffer,
    * but upcast to the appropriate subclass of Buffer using, for example,
    * ByteBuffer.asFloatBuffer. The view calls are somewhat dangerous because
    * they create a view with proper endianness for Java (which uses the native
    * endianness internally) but they can also change the endianness of
    * _byteBuffer. This is subtle, so read the Java documentation carefully if
    * you need to change this.
    */
  def directBuffer: ByteBuffer = _directBuffer.asInstanceOf[ByteBuffer]

  /** Make a copy of the memory.
    *
    * This uses an indirect buffer rather than the direct buffer of the parent
    * so that the garbage collector can dispose of it easily.
    *
    * @return A copy of the memory
    */
  def copy: ColorFieldMemory = {
    val newMemory = new ColorFieldMemory(fieldType, IndirectBuffer)
    this.copyTo(newMemory)
    newMemory
  }

  /** An iterator over all pixel-bytes in the field (does not include an alpha channel), in row-major and r-g-b byte order. */
  def iterator = new Iterator[Byte] {
    private var pixelIndex = 0
    private var componentIndex = 0
    private val numPixels = rows * columns
    private val buffer = directBuffer.duplicate
    buffer.rewind

    def hasNext = pixelIndex < numPixels
    def next(): Byte = {
      val value = buffer.get(pixelIndex * 4 + componentIndex)
      componentIndex += 1
      // Skip 4th component (the alpha channel)
      if (componentIndex == 3) {
        componentIndex = 0
        pixelIndex += 1
      }
      value
    }
  }

  /** Read the value at (`row`, `col`) in a 2D color field into `out`. */
  def read(row: Int, col: Int, out: Pixel) {
    require(dimensions == 2)
    val offset = (row * paddedColumns + col) * PixelSize
    out.red = directBuffer.get(offset)
    out.green = directBuffer.get(offset + 1)
    out.blue = directBuffer.get(offset + 2)
  }

  /** Write `out` from a 2D color field at (`row`, `col`). */
  def write(row: Int, col: Int, out: Pixel) {
    require(dimensions == 2)
    val offset = (row * paddedColumns + col) * PixelSize
    directBuffer.put(offset, out.red)
    directBuffer.put(offset + 1, out.green)
    directBuffer.put(offset + 2, out.blue)
  }

  /** Init memory with values produced by `generator`. */
  private[cogx] def init(generator: (Int, Int) => Pixel) {
    require(dimensions == 2)
    val rows = fieldType.fieldShape(0)
    val columns = fieldType.fieldShape(1)
    for (row <- 0 until rows; col <- 0 until columns)
      write(row, col, generator(row, col))
  }

  /** Compute the L-infinity norm on the difference of `this` and `that`.
    *
    * @param that AbstractFieldMemory to compare to `this`
    * @return L-infinity error
    */
  def compareLinf(that: FieldReader): Float = {
    require(fieldType.equals(that.fieldType))
    require(that.isInstanceOf[ColorFieldReader])
    require(dimensions == 2)

    val p1 = new Pixel(0, 0, 0)
    val p2 = new Pixel(0, 0, 0)

    var err = 0f

    for (i <- 0 until fieldShape(0)) {
      for (j <- 0 until fieldShape(1)) {
        read(i, j, p1)
        that.asInstanceOf[ColorFieldReader].read(i, j, p2)
        err = err.max(math.abs(p1.redFloat - p2.redFloat))
                .max(math.abs(p1.greenFloat - p2.greenFloat))
                .max(math.abs(p1.blueFloat - p2.blueFloat))
      }
    }

    err
  }

  /** Print out the field for debugging. */
  def print() {
    throw new RuntimeException("not implemented yet")
  }
}

/** Factory for creating ColorFieldMemories.
  *
  * THIS IS ONLY USED FOR TESTING. NOT EXPORTED TO USERS.
  */
object ColorFieldMemory {

  /** Create a stand-alone ColorFieldMemory for testing.
    *
    * @param rows Rows in field.
    * @param columns Columns in field.
    * @param f Function which supplies a pixel for every point in the field.
    * @return Initialized color field memory.
    */
  def apply(rows: Int, columns: Int, f:(Int, Int) => Pixel): ColorFieldMemory = {
    val fieldType =
      new FieldType(Shape(rows, columns), Shape(3), Uint8Pixel)
    val bufferType = IndirectBuffer
    val memory = new ColorFieldMemory(fieldType, bufferType)
    for (r <- 0 until rows; c <- 0 until columns)
      memory.write(r, c, f(r, c))
    memory
  }
}