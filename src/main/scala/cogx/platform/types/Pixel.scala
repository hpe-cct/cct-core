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

package cogx.platform.types

/** A pixel has red, green and blue color channels, each of which is
  * represented by a byte. This is readable and writable, mimicking a vector.
  *
  * @param r Red channel value, as a byte.
  * @param g Green channel value, as a byte.
  * @param b Blue channel value, as a byte.
  *
  * @author Greg Snider
  */

class Pixel(r: Byte, g: Byte, b: Byte) {
  import Pixel._
  /** Red component. */
  private var _red: Byte = r
  /** Green component. */
  private var _green: Byte = g
  /** Blue component. */
  private var _blue: Byte = b

  /** Construct a pixel from bytes. Useful for interfacing with byte-oriented
    * hardware.
    *
    * @param red Red channel as a Float in (0.0f, 1.0f)
    * @param green Green channel as a Float in (0.0f, 1.0f)
    * @param blue Blue channel as a Float in (0.0f, 1.0f)
    * @return Pixel.
    */
  def this(red: Float, green: Float, blue: Float) =
    this(Pixel.toByte(red), Pixel.toByte(green), Pixel.toByte(blue))

  def this(r: Int, g: Int, b: Int) =
    this(Pixel.fromInt(r), Pixel.fromInt(g), Pixel.fromInt(b))

  /** Construct a black pixel. */
  def this() =
    this(0, 0, 0)

  /** Red channel, as a Float in (0.0f, 1.0f). */
  def redFloat: Float = toFloat(_red)

  /** Green channel, as a Float in (0.0f, 1.0f). */
  def greenFloat: Float = toFloat(_green)

  /** Blue channel, as a Float in (0.0f, 1.0f). */
  def blueFloat: Float = toFloat(_blue)

  /** Red channel, as an Int in (0, 255). */
  def redInt: Int = toInt(_red)

  /** Green channel, as an Int in (0, 255). */
  def greenInt: Int = toInt(_green)

  /** Blue channel, as an Int in (0, 255). */
  def blueInt: Int = toInt(_blue)

  /** Red channel, as a Byte (which is signed, i.e. in the range [-128, 127). */
  def red: Byte = _red

  /** Green channel, as a Byte (which is signed, i.e. in the range [-128, 127). */
  def green: Byte = _green

  /** Blue channel, as a Byte (which is signed, i.e. in the range [-128, 127). */
  def blue: Byte = _blue

  /** Write red channel with `value`. */
  def red_=(value: Byte) {_red = value}

  /** Write green channel with `value`. */
  def green_=(value: Byte) {_green = value}

  /** Write blue channel with `value`. */
  def blue_=(value: Byte) {_blue = value}

  /** Convert a color channel to a float in (0.0f, 1.0f). */
  private def toFloat(color: Byte): Float = {
    (color.toInt & 0xff).toFloat / MaxByte
  }


  /** Compare two pixels for value equality. */
  override def equals(that: Any): Boolean = {
    that match {
      case p: Pixel =>
        (_red == p._red) && (_green == p._green) && (_blue == p._blue)
      case _ => false
    }
  }

  /** Required because of equals override. Since this is a mutable class, we
    * we use a hashcode that puts all pixels in the same bucket. These means
    * sets of pixels are extremely inefficient, but would work correctly.
    */
  override def hashCode = 0

  override def toString = "(" + redInt + "," + greenInt + "," + blueInt + ")"
}

/** Companion for Pixel class to help constructors. */
object Pixel {
  /** Maximum value for a color channel when encoded in an unsigned byte. */
  private val MaxByte = 255

  /** Convert an unsigned Int to a signed Byte. */
  private def fromInt(color: Int): Byte = {
    require(color >= 0 && color <= MaxByte)
    color.toByte
  }

  /** Convert a (sadly) signed Byte to an unsigned Int. */
  private def toInt(color: Byte): Int = {
    color.toInt & MaxByte
  }

  /** Convert a float color value to an encoded byte. */
  private def toByte(value: Float): Byte = {
    require(value >= 0 && value <= 1f,"Out of bounds color channel: " + value)
    if (value == 1.0f)
      MaxByte.toByte
    else
      ((value * 256).toInt & MaxByte).toByte
  }
}
