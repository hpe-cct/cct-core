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

package cogx.reference

import memory.{RefFieldMemory, RefImageMemory}
import cogx.cogmath.algebra.real.{Scalar, ArrayMath}
import cogx.cogmath.geometry.Shape
import cogx.platform.types.{FieldType, Pixel}
import cogx.platform.types.ElementTypes.Float32
import java.util.Random

/** Special field used to represent color images.
  *
  * @param data The pixel buffer holding the field data.
  *
  * @author Greg Snider
  */
class RefColorField private[cogx] (val data: RefImageMemory)
        extends RefField(data.fieldType)
        with ArrayMath
{
  /** Create a "height" x "width" color field holding pixel "data" in ARGB
    * format
    */
  def this(height: Int, width: Int, data: Array[Byte]) =
    this(new RefImageMemory(Shape(height, width), data))

  def this(shape: Shape, data: Array[Byte]) =
    this(new RefImageMemory(shape, data))

  /** Extract a color image of size "height" x "width" from a graphics hardware
    * buffer. Hardware uses an Int to encode the R, G, B components of a single
    * pixel in ARGB format.
    */
  /*
  def this(height: Int, width: Int, hardwareBuffer: IntBuffer) =
    this({
      // Flip image upside down. For some reason the images we get
      // from our virtual environment create them this way.
      val pixels = new Array[Int](height * width)
      val buffer = hardwareBuffer.asReadOnlyBuffer
      buffer.rewind
      for (y <- 0 until height) {
        val outOffset = (height - y - 1) * width
        buffer.get(pixels, outOffset, width)
      }
      // Write the pixels into an image memory
      val memory = new RefImageMemory(Shape(height, width))
      for (row <- 0 until height; col <- 0 until width) {
        val offset = row * width + col
        val intPixel = pixels(offset)
        val realPixel = Pixel(intPixel)
        memory.write(row, col, realPixel)
      }
      memory
    })
    */


  /** Create a "height" x "width" field with pixels specified by "f". */
  def this(height: Int, width: Int, f: (Int, Int) => (Float, Float, Float)) =
    this({
      val memory = new RefImageMemory(Shape(height, width))
      for (row <- 0 until height; col <- 0 until width) {
        val rgb = f(row, col)
        val red = rgb._1
        val green = rgb._2
        val blue = rgb._3
        val pixel = new Pixel(red, green, blue)
        memory.write(row, col, pixel)
      }
      memory
    })

  /** Create a "depth" x "height" x "width" field with pixels specified by "f". */
  def this(depth: Int, height: Int, width: Int, f: (Int, Int, Int) => (Float, Float, Float)) =
    this({
      val memory = new RefImageMemory(Shape(depth, height, width))
      for (z <- 0 until depth; row <- 0 until height; col <- 0 until width) {
        val rgb = f(z, row, col)
        val red = rgb._1
        val green = rgb._2
        val blue = rgb._3
        val pixel = new Pixel(red, green, blue)
        memory.write(z, row, col, pixel)
      }
      memory
    })


  /** Create a "height" x "width" black image. */
  def this(height: Int, width: Int) =
    this({
      val memory = new RefImageMemory(Shape(height, width))
      for (row <- 0 until height; col <- 0 until width)
        memory.write(row, col, new Pixel)
      memory
    })

  /** Create a color field from red, green, and blue scalar fields. */
  def this(red: RefScalarField, green: RefScalarField, blue: RefScalarField) =
    this({
      require(red.dimensions == 2)
      require(green.rows == red.rows && green.columns == red.columns)
      require(blue.rows == red.rows && blue.columns == red.columns)
      val height = red.rows
      val width = red.columns
      val memory = new RefImageMemory(Shape(height, width))
      for (row <- 0 until height; col <- 0 until width) {
        val r = red.read(row, col)
        val g = green.read(row, col)
        val b = blue.read(row, col)
        memory.write(row, col, new Pixel(r, g, b))
      }
      memory
    })

  /*
  // Scalar algebra:
  def +(that: Float) = map(_ + that)
  def -(that: Float) = map(_ - that)
  def *(that: Float) = map(_ * that)
  def /(that: Float) = map(_ / that)
  def unary_-() = this * -1f
  private[cogx] def reciprocal = map(pixel => pixel.map(1f/_))

  def +(that: Pixel) = map(_ + that)
  def -(that: Pixel) = map(_ - that)
  def *(that: Pixel) = map(_ * that)
  def /(that: Pixel) = map(_ / that)

  // Field algebra:
  def +(that: RefColorField) = combine(that, _ + _)
  def -(that: RefColorField) = combine(that, _ - _)
  def *(that: RefColorField) = combine(that, _ * _)
  def /(that: RefColorField) = combine(that, _ / _)

  // Map, preserving alpha as 1.0f
  def map(f: (Pixel) => Pixel): RefColorField = {
    val result = new RefImageMemory(this.fieldType.fieldShape)
    for (row <- 0 until height; col <- 0 until width) {
      val pixel = data.read(row, col)
      result.write(row, col, f(pixel))
    }
    new RefColorField(result)
  }

  // Combine, preserving alpha as 1.0f
  def combine(that: RefColorField, f: (Pixel, Pixel) => Pixel): RefColorField = {
    val result = new RefImageMemory(this.fieldType.fieldShape)
    for (row <- 0 until height; col <- 0 until width) {
      val pixel1 = data.read(row, col)
      val pixel2 = that.data.read(row, col)
      result.write(row, col, f(pixel1, pixel2))
    }
    new RefColorField(result)
  }
  */

  /** The height of the color field. */
  def height = fieldShape(0)

  /** The width of the color field. */
  def width = fieldShape(1)

  /** Return the field with each pixel packed in an Int. */
  //def packedBuffer: Array[Int] = {
  //  data.asPixelArray
  //}

  /** A very low-level mechanism for mapping color fields.
    * The function "f" takes the A, R, G, B fields of the current pixel, with
    * each field having a value in the interval [0, 255], and returns new
    * A, R, G, and B values for the pixel.
    */
  def map(f: (Int, Int, Int, Int) => (Int, Int, Int, Int)): RefColorField = {
    require(dimensions == 2)
    val mapped = new RefImageMemory(fieldType.fieldShape)
    for (row <- 0 until height; col <- 0 until width) {
      val pixel = data.read(row, col)
      val alphaInt = (255).toInt
      val redInt = (pixel.red * 255).toInt
      val greenInt = (pixel.green * 255).toInt
      val blueInt = (pixel.blue * 255).toInt
      val (alpha, red, green, blue) = f(alphaInt, redInt, greenInt, blueInt)
      mapped.write(row, col, new Pixel(red / 255f, green / 255f, blue / 255f))
    }
    new RefColorField(mapped)
  }

  /** Alpha blend "top" on top of "this". The alpha value of "top" determines
    * the blending. The result has an alpha value of 1 everywhere.
    */
  /*
  def superimpose(top: RefColorField): RefColorField = {
    require(this.fieldShape == top.fieldShape)
    require(dimensions == 2)
    val result = new RefImageMemory(fieldType.fieldShape)
    for (row <- 0 until height; col <- 0 until width) {
      val thisPixel = data.read(row, col)
      val topPixel = top.data.read(row, col)
      val red = topPixel.red * alpha + thisPixel.red * (1 - alpha)
      val green = topPixel.green * alpha + thisPixel.green * (1 - alpha)
      val blue = topPixel.blue * alpha + thisPixel.blue * (1 - alpha)
      result.write(row, col, new Pixel(red, green, blue))
    }
    new RefColorField(result)
  }
  */

  /** Make a clone of this RefColorField. */
  //override def copy = new RefColorField(data.copy)

  /** INTERNAL USE ONLY--to speed up transfers to GPU. */
  private[cogx] def asRawArray: Array[Float] = {
    throw new RuntimeException("illegal call") //data.asRawArray
  }

  /** INTERNAL USE ONLY--to speed up transfers to GPU. */
  //private[cogx] def asPixelArray: Array[Int] = {
  //  data.asPixelArray
  //}

  /** Blend the component colors of each pixel by the given weights to make a "grayscale" RefScalarField. */
  def toGray(redWeight: Float, greenWeight: Float, blueWeight: Float): RefScalarField = {
    val resultType = new FieldType(this.fieldType.fieldShape, Shape(), Float32)
    val result = new RefFieldMemory(resultType)
    for (row <- 0 until height; col <- 0 until width) {
      val pixel = data.read(row, col)
      val gray = pixel.redFloat * redWeight + pixel.greenFloat * greenWeight +
              pixel.blueFloat * blueWeight
      result.write(new Scalar(gray), row, col)
    }
    new RefScalarField(result)
  }

  /** Extract the luminance from a color field. */
  def luminance: RefScalarField = toGray(0.3811f, 0.5783f, 0.0402f)

  /** Extract the indexed color field: 0 => red, 1 => green, 2 => blue
    */
  private def extractColorPlane(index: Int): RefScalarField = {
    index match {
      case 0 => toGray(1, 0, 0)   // red
      case 1 => toGray(0, 1, 0)   // green
      case 2 => toGray(0, 0, 1)   // blue
      case _ => throw new RuntimeException("Illegal color plane index")
    }
  }

  /** Extract the red color plane from this color field. */
  def red: RefScalarField = extractColorPlane(0)

  /** Extract the green color plane from this color field. */
  def green: RefScalarField = extractColorPlane(1)

  /** Extract the blue color plane from this color field. */
  def blue: RefScalarField = extractColorPlane(2)


  def subfield(in0: Range): RefColorField =
    throw new RuntimeException("Illegal subfield call on RefColorField")


  def subfield(in0: Range, in1: Range, in2: Range): RefColorField =
    throw new RuntimeException("Illegal subfield call on RefColorField")

  /** Extract a subfield where the 2 field indices vary over "in0", "in1"
    * resulting in a smaller, 2-dimensional color field.
    */
  def subfield(in0: Range, in1: Range): RefColorField = {
    require(fieldType.fieldShape.dimensions == 2)
    val resultRows = in0.length
    val resultColumns = in1.length
    val rowOffset = in0.start
    val colOffset = in1.start
    val resultMemory = new RefImageMemory(Shape(resultRows, resultColumns))
    for (row <- 0 until resultRows; col <- 0 until resultColumns) {
      val pixel = data.read(row + rowOffset, col + colOffset)
      resultMemory.write(row, col, pixel)
    }
    new RefColorField(resultMemory)
  }

  def downsample(factor: Int = 2, phase:Int = 0): RefColorField =
    new RefColorField(this.red.downsample(factor, phase),
      this.green.downsample(factor, phase), this.blue.downsample(factor, phase))

  /** Compare "this" and "that" for approximate equality. */
  def ~==(that: RefColorField): Boolean = {
    data ~== that.data
  }

  // Value equality:
  override def equals(that: Any): Boolean = {
    that match {
      case other: RefColorField => data == other.data
      case _ => false
    }
  }
  override def hashCode: Int = data.hashCode

  /** Print out a color field for debugging. */
  def print {
    require(fieldType.fieldShape.dimensions == 2)
    val rows = fieldType.fieldShape(0)
    val columns = fieldType.fieldShape(1)
    printf("RefColorField (%d x %d):\n", rows, columns)
    for (row <- 0 until rows) {
      for (col <- 0 until columns) {
        val pixel = data.read(row, col)
        printf(" RGB(%6.3f %6.3f %6.3f)",
          pixel.redFloat, pixel.greenFloat, pixel.blueFloat)
      }
      println
    }
  }
}

/** Companion object for RefColorField
  */
object RefColorField {
  val Colors = 3

  /** Create a "height" x "width" color field, where the pixels are supplied
    * in row-major order (1st row, left-to-right; 2nd row, left-to-right; ...)
    * by the iterator "f".
    */
  def apply(height: Int, width: Int, f: Iterator[Pixel]): RefColorField = {
    val memory = new RefImageMemory(Shape(height, width))
    for (row <- 0 until height)
      for (col <- 0 until width)
        memory.write(row, col, f.next)
    new RefColorField(memory)
  }

  /** Create a color field where "f" supplies the A, R, G, B components
    */
  def apply(height: Int, width: Int,
            f: (Int, Int) => (Float, Float, Float, Float)): RefColorField =
  {
    val memory = new RefImageMemory(Shape(height, width))
    for (y <- 0 until height; x <- 0 until width) {
      val (alpha, red, green, blue) = f(y, x)
      memory.write(y, x, new Pixel(red, green, blue))
    }
    new RefColorField(memory)
  }

  /** Create a black color field with the given "shape". */
  def apply(shape: Shape): RefColorField = {
    require(shape.dimensions == 2 || shape.dimensions == 3)
    new RefColorField(shape(0), shape(1))
  }

  /** Create a random "rows" x "columns" RefColorField. */
  def random(rows: Int, columns: Int): RefColorField = {
    val r = new Random
    new RefColorField(rows, columns,
      (row, col) => (r.nextFloat, r.nextFloat, r.nextFloat))
  }

  /** Create a random RefColorField with given "shape". */
  def random(shape: Shape): RefColorField = {
    require(shape.dimensions == 2)
    random(shape(0), shape(1))
  }
}

