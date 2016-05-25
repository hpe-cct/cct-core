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

package cogx.reference.memory

import cogx.cogmath.geometry.Shape
import cogx.platform.types.{Pixel, FieldType}
import cogx.platform.types.ElementTypes.Uint8Pixel


/** Memory for representing low-resolution (8 bits per color channel)
  * color images (using RGBA format, required by OpenCL as the only format
  * guaranteed to be supported).
  *
  * @param fieldShape The type of the image.
  * @param initData The data containing the field's state. If this is not specified,
  *        it will be initialized with all zeroes.
  *
  * @author Greg Snider
  */
class RefImageMemory(val fieldShape: Shape, initData: Array[Byte] = null)
{
  require(fieldShape.dimensions == 2 || fieldShape.dimensions == 3)
  private val ColorChannels = 4
  private val pixels = fieldShape.points
  val fieldType = new FieldType(fieldShape, Shape(ColorChannels), Uint8Pixel)
  if (initData != null)
    require(initData.length == pixels * ColorChannels)
  private val dimensions = fieldShape.dimensions
  private val slices = if (dimensions == 2) 0 else fieldShape(0)
  private val rows = if (dimensions == 2) fieldShape(0) else fieldShape(1)
  private val columns = if (dimensions == 2) fieldShape(1) else fieldShape(2)
  private val AlphaByte = 0xff.toByte

  /** Data held in field. */
  private val data = if (initData != null)
    initData
  else
    new Array[Byte](pixels * ColorChannels)

  def read(row: Int, col: Int): Pixel = {
    require(dimensions == 2)
    val index = offset(row, col)
    //new Pixel(data(index), data(index + 1), data(index + 2), data(index + 3))
    val pixel = new Pixel
    pixel.red = data(index)
    pixel.green = data(index + 1)
    pixel.blue = data(index + 2)
    pixel
  }

  def read(slice: Int, row: Int, col: Int): Pixel = {
    require(dimensions == 3)
    val index = offset(slice, row, col)
    //new Pixel(data(index), data(index + 1), data(index + 2), data(index + 3))
    val pixel = new Pixel
    pixel.red = data(index)
    pixel.green = data(index + 1)
    pixel.blue = data(index + 2)
    pixel
  }

  def write(row: Int, col: Int, pixel: Pixel) {
    require(pixel != null)
    require(dimensions == 2)
    val index = offset(row, col)
    data(index) = pixel.red
    data(index + 1) = pixel.green
    data(index + 2) = pixel.blue
    data(index + 3) = AlphaByte
  }

  def write(slice: Int, row: Int, col: Int, pixel: Pixel) {
    require(dimensions == 3)
    val index = offset(slice, row, col)
    data(index) = pixel.red
    data(index + 1) = pixel.green
    data(index + 2) = pixel.blue
    data(index + 3) = AlphaByte
  }

  private def offset(row: Int, col: Int) =
    (row * columns + col) * ColorChannels

  private def offset(slice: Int, row: Int, col: Int) =
    (slice * rows * columns + row * columns + col) * ColorChannels

  /////////////////////////////////////////////////////////////////////////////
  //  Copiers
  /////////////////////////////////////////////////////////////////////////////

  /** Make an exact copy of this memory block. */
  /*
  def copy: RefImageMemory = {
    val dataCopy = new Array[Float](data.length)
    Array.copy(data, 0, dataCopy, 0, data.length)
    new RefImageMemory(fieldShape, dataCopy)
  }
  */

  /////////////////////////////////////////////////////////////////////////////
  // Comparison
  /////////////////////////////////////////////////////////////////////////////
  /** Test "this" and "other" for deep equality. Allows "==" to work. Note that
    * this skips over the padded values within the field memory.
    */
  override def equals(other: Any): Boolean =
    other match {
      case that: RefImageMemory =>
        if (fieldType == that.fieldType) {
          for (i <- 0 until data.length) {
            if (this.data(i) != that.data(i))
              return false
          }
          true
        } else
          false
      case _ => false
    }

  override def hashCode: Int = {
    fieldType.fieldShape.points * fieldType.tensorShape.points
  }

  /** Test "this" and "that" for approximate equality. Note that this skips
    * over the padded values within the field memory.
    */
  def ~==(that: RefImageMemory): Boolean = {
    if (!(this.fieldType == that.fieldType))
      false
    else {
      for (i <- 0 until data.length)
        if (!approxEqual(data(i), that.data(i)))
          return false
      true
    }
  }

  /** Return true if "x" and "y" are approximately equal. */
  protected def approxEqual(x: Float, y: Float): Boolean = {
    if (math.abs(x - y) > 0.001f) {
      // Perhaps x and y are close enough in a relative-error sense?
      if ((y == 0) || math.abs(x/y - 1f) > 1e-5)
        return false
    }
    true
    // Legacy version below.  The above, stolen from FieldMemory works better.  -RJC

    //    // eps exponent was -15 .  Old GT200 architecture failed with -13 on log test.  -RJC
    //    val eps = math.pow(2.0, -12.0)
    //    if (x == 0 && y.abs < 10 * eps)
    //      true
    //    else if (y == 0 && x.abs < 10 * eps)
    //      true
    //    else
    //      (x - y).abs < 10 * eps * (x.abs max y.abs)
  }

  //private[cog] def asRawArray: Array[Float] = data

  /** Compress the RGBA float image to an ARGB Int image (for display). */
  /*
  private[cog] def asPixelArray: Array[Int] = {
    val argb = new Array[Int](pixels)
    for (index <- 0 until pixels) {
      argb(index) = IntPixelARGB.encode(
        data(4 * index + 3),  // alpha
        data(4 * index + 0),  // red
        data(4 * index + 1),  // green
        data(4 * index + 2))  // blue
    }
    argb
  }
  */
}

