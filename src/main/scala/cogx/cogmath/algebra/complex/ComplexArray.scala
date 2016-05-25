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

package cogx.cogmath.algebra.complex

import collection.generic.CanBuildFrom
import collection.mutable.{ArrayBuffer, Builder, IndexedSeq}
import ComplexArray._
import cogx.cogmath.algebra.real.VirtualArray

/** An array of complex numbers; unlike other collections in this package,
  * ComplexArrays are mutable.
  *
  * Complex numbers are stored in a non-interleaved format in an array of floats:
  * real parts in the first half of the array, followed by imaginary parts in the
  * second half of the array.
  *
  * @param data Data for the array.
  *
  * @author Greg Snider
  */
@SerialVersionUID(-3768422674473260210L)
class ComplexArray (private val data: Array[Float])
        extends IndexedSeq[Complex] with Serializable
{
  require(data.length % 2 == 0)

  /** Create a zero-filled complex array of length "size". */
  def this(size: Int) = this(new Array[Float](2 * size))

  /** Convert an array of Complex to a complex array. */
  def this(values: Array[Complex]) = this({
    val len = values.length
    val data = new Array[Float](2 * len)
    for (i <- 0 until len) {
      val complex = values(i)
      data(i) = complex.real
      data(i + len) = complex.imaginary
    }
    data
  })

  /** Convert an Iterable[Complex] to a complex array. */
  def this(length: Int, values: Iterable[Complex]) = this({
    val data = new Array[Float](2 * length)
    val reader = values.iterator
    for (i <- 0 until length) {
      require(reader.hasNext,"Length mismatch in Iterable[Complex]")
      val complex = reader.next
      data(i) = complex.real
      data(i + length) = complex.imaginary
    }
    require(!reader.hasNext,"Length mismatch in Iterable[Complex]")
    data
  })

  /** Convert a sequence of complex numbers to a complex array. */
  def this(c: Complex*) = this(c.toArray)

  /** Peek at data. This is DANGEROUS and exists only for use by the GPU.
    *
    * APPLICATIONS MUST NOT USE THIS.
    */
  def asRawArray: Array[Float] = data

  /** Create a deep copy of this complex array. */
  def copy: ComplexArray = {
    val newData = new Array[Float](data.length)
    Array.copy(data, 0, newData, 0, data.length)
    new ComplexArray(newData)
  }

  /** Copy from "this" to "target". */
  def copyTo(target: ComplexArray) {
    require(this.length == target.length)
    Array.copy(data, 0, target.data, 0, data.length)
  }


  /** Copy a contiguous slice of "this" complex array spanning indices
    * ("from" until "until") into the complex array "to" starting at index
    * "toStart"
    */
  def sliceAndCopyTo(from: Int, until: Int, to: ComplexArray, toStart: Int) {
    require(until > from)
    val copyLength = until - from
    Array.copy(data, from, to.data, toStart, copyLength)
    Array.copy(data, from + length, to.data, toStart + to.length, copyLength)
  }

  /** Extract subarray from index "from" (inclusive) to "until" (exclusive). */
  override def slice(from: Int, until: Int) = {
    val newArray = new ComplexArray(until - from)
    sliceAndCopyTo(from, until, newArray, 0)
    newArray
  }

  /** Number of elements in the array. */
  def length = data.length / 2

  /** Real complex number at location "index" in the array. */
  def apply(index: Int) = new Complex(real(index), imaginary(index))

  /** Get the real part of the complex array. */
  def viewRealPart =
    new VirtualArray {
      def apply(index: Int) = data(index)
      def update(index: Int, value: Float) {data(index) = value}
      def length = data.length / 2
    }

  /** Get the imaginary part of the complex array. */
  def viewImaginaryPart =
    new VirtualArray {
      def apply(index: Int) = data(index + length)
      def update(index: Int, value: Float) {data(index + length) = value}
      def length = data.length / 2
    }

  /** Copy and return the real part of the array. */
  def realPart: Array[Float] = {
    val realData = new Array[Float](data.length / 2)
    Array.copy(data, 0, realData, 0, length)

    realData
  }

  /** Copy and return the real part of the array. */
  def imaginaryPart: Array[Float] = {
    val imaginaryData = new Array[Float](data.length / 2)
    Array.copy(data, length, imaginaryData, 0, length)

    imaginaryData
  }

  /** Get the real part of the complex number at location "index". */
  def real(index: Int): Float =
    data(index)

  /** Get the imaginary part of the complex number at location "index". */
  def imaginary(index: Int): Float =
    data(index + length)

  /** Write location "index" with "value". */
  def update(index: Int, value: Complex) {
    data(index) = value.real
    data(index + length) = value.imaginary
  }

  /** Write location "index" with (real, imaginary). */
  def update(index: Int, real: Float, imaginary: Float) {
    data(index) = real
    data(index + length) = imaginary
  }

  /** Write location "index" with Float "value".  Hopefully higher performing
    * than invoking an implicit conversion to a Complex.*/
  def update(index: Int, realOnly: Float) {
    data(index) = realOnly
    data(index + length) = 0.0f
  }

  /** Map this array using "f". */
  def mapComplex(f: Complex => Complex): ComplexArray = {
    val newData = new Array[Float](data.length)
    for (i <- 0 until length) {
      val mapped = f(Complex(real(i), imaginary(i)))
      newData(i) = mapped.real
      newData(i + length) = mapped.imaginary
    }
    new ComplexArray(newData)
  }

  /** Map this array using "f". */
  def mapToReal(f: Complex => Float): Array[Float] = {
    val newData = new Array[Float](data.length / 2)
    for (i <- 0 until length) {
      newData(i) = f(Complex(real(i), imaginary(i)))
    }
    newData
  }

  /** Reduce this array. */
  /*
  def reduceLeft(f: (Complex, Complex) => Complex): Complex = {
    var result = this(0)
    for (i <- 1 until length)
      result = f(result, this(i))
    result
  }
  */

  /** `this` + `that`  */
  def +(that: ComplexArray): ComplexArray = {
    require(that.length == this.length)
    val result = new Array[Float](data.length)
    for (i <- 0 until data.length)
      result(i) = this.data(i) + that.data(i)
    new ComplexArray(result)
  }

  /** `this` - `that`  */
  def -(that: ComplexArray): ComplexArray = {
    require(that.length == this.length)
    val result = new Array[Float](data.length)
    for (i <- 0 until data.length)
      result(i) = this.data(i) - that.data(i)
    new ComplexArray(result)
  }

  /** `this` :* `that`  (element-wise multiply) */
  def :*(that: ComplexArray): ComplexArray = {
    require(that.length == this.length)
    val result = new Array[Float](data.length)
    for (i <- 0 until length) {
      val thisReal = this.data(i)
      val thisImaginary = this.data(i + length)
      val thatReal = that.data(i)
      val thatImaginary = that.data(i + length)
      val real = thisReal * thatReal - thisImaginary * thatImaginary
      val imag = thisReal * thatImaginary + thisImaginary * thatReal
      result(i) = real
      result(i + length) = imag
    }
    new ComplexArray(result)
  }

  /** Compute the complex dot product of `this` and `that`. */
  def dotProduct(that: ComplexArray): Complex = {
    require(that.length == this.length)
    var realResult = 0f
    var imaginaryResult = 0f
    for (i <- 0 until length) {
      val thisReal = this.data(i)
      val thisImaginary = this.data(i + length)
      val thatReal = that.data(i)
      val thatImaginary = that.data(i + length)
      val real = thisReal * thatReal - thisImaginary * thatImaginary
      val imag = thisReal * thatImaginary + thisImaginary * thatReal
      realResult += real
      imaginaryResult += imag
    }
    new Complex(realResult, imaginaryResult)
  }

  /** `this` :/ `that`  (element-wise division) */
  def :/(that: ComplexArray): ComplexArray = {
    require(that.length == this.length)
    val result = new Array[Float](data.length)
    for (i <- 0 until length) {
      val thisReal = this.data(i)
      val thisImaginary = this.data(i+ length)
      val thatReal = that.data(i)
      val thatImaginary = that.data(i + length)
      val quotient =
        Complex(thisReal, thisImaginary) / Complex(thatReal, thatImaginary)
      result(i) = quotient.real
      result(i + length) = quotient.imaginary
    }
    new ComplexArray(result)
  }

  def +(that: Float): ComplexArray = {
    val result = new Array[Float](data.length)
    val value = that.toFloat
    for (i <- 0 until length) {
      result(i) = data(i) + value
      Array.copy(data, length, result, length, length)
    }
    new ComplexArray(result)
  }

  def -(that: Float): ComplexArray = {
    val result = new Array[Float](data.length)
    val value = that.toFloat
    for (i <- 0 until length) {
      result(i) = data(i) - value
      Array.copy(data, length, result, length, length)
    }
    new ComplexArray(result)
  }

  def *(that: Float): ComplexArray = {
    val result = new Array[Float](data.length)
    val value = that.toFloat
    for (i <- 0 until result.length)
      result(i) = data(i) * value
    new ComplexArray(result)
  }

  def /(that: Float): ComplexArray = {
    val result = new Array[Float](data.length)
    val value = that.toFloat
    for (i <- 0 until result.length)
      result(i) = data(i) / value
    new ComplexArray(result)  }

  def +(that: Complex): ComplexArray = {
    val result = new Array[Float](data.length)
    val real = that.real
    val imag = that.imaginary
    for (i <- 0 until length) {
      result(i) = data(i) + real
      result(i + length) = data(i + length) + imag
    }
    new ComplexArray(result)
  }

  def -(that: Complex): ComplexArray = {
    val result = new Array[Float](data.length)
    val real = that.real
    val imag = that.imaginary
    for (i <- 0 until length) {
      result(i) = data(i) - real
      result(i + length) = data(i + length) - imag
    }
    new ComplexArray(result)
  }

  def *(that: Complex): ComplexArray = {
    val thatReal = that.real
    val thatImaginary = that.imaginary
    val result = new Array[Float](data.length)
    for (i <- 0 until length) {
      val thisReal = this.data(i)
      val thisImaginary = this.data(i + length)
      val real = thisReal * thatReal - thisImaginary * thatImaginary
      val imag = thisReal * thatImaginary + thisImaginary * thatReal
      result(i) = real
      result(i + length) = imag
    }
    new ComplexArray(result)
  }

  def /(that: Complex): ComplexArray = {
    mapComplex(_ / that)
  }

  def dot(that: ComplexArray): Complex = {
    require(that.length == this.length)
    var realSum = 0f
    var imaginarySum = 0f
    for (i <- 0 until length) {
      val thisReal = this.data(i)
      val thisImaginary = this.data(i + length)
      val thatReal = that.data(i)
      val thatImaginary = that.data(i + length)
      val real = thisReal * thatReal - thisImaginary * thatImaginary
      val imag = thisReal * thatImaginary + thisImaginary * thatReal
      realSum += real
      imaginarySum += imag
    }
    Complex(realSum, imaginarySum)
  }

  /** Test `this` and `other` for deep equality. Allows `==` to work. */
  override def equals(other: Any): Boolean =
    other match {
      case that: ComplexArray =>
        if ((that canEqual this) && (that.length == this.length))
        {
          for (i <- 0 until data.length) {
            if (this.data(i) != that.data(i))
              return false
          }
          true
        }
        else
          false
      case _ => false
    }

  /** Helper for equals. */
  override def canEqual(other: Any): Boolean = other.isInstanceOf[ComplexArray]

  /** Required because of overriding equals. */
  override def hashCode: Int = {
    // Since ComplexArrays aren't likely to go into sets, we do a simple hash.
    (length + 41)
  }

  /** Compare `this` and `that` for approximate equality. */
  def ~==(that: ComplexArray): Boolean = {
    if (that.length != this.length)
      false
    else {
      for (i <- 0 until length)
        if (!(this(i) ~== that(i)))
          return false
      true
    }
  }
}

/**
 * Companion object supplying concatenation.
 */
object ComplexArray {
  /** Convert a real array to a complex array. */
  def fromReal(real: Array[Float]): ComplexArray = {
    val data = new Array[Float](2 * real.length)
    for (i <- 0 until real.length)
      data(i) = real(i).toFloat
    new ComplexArray(data)
  }

  /** Convert a real array and imaginary array to a complex array. */
  def fromRealImaginary(real: Array[Float],
                        imaginary: Array[Float]): ComplexArray =
  {
    require(real.length == imaginary.length)
    val len = real.length
    val data = new Array[Float](2 * len)
    for (i <- 0 until len) {
      data(i) = real(i)
      data(i + len) = imaginary(i)
    }
    new ComplexArray(data)
  }

  def fromPolar(magnitude: Array[Float], phase: Array[Float]): ComplexArray = {
    require(magnitude.length == phase.length)
    val len = magnitude.length
    val data = new Array[Float](2 * len)
    for (i <- 0 until len) {
      val r = magnitude(i)
      val theta = phase(i)
      val real = r * math.cos(theta).toFloat
      val imaginary = r * math.sin(theta).toFloat
      data(i) = real
      data(i + len) = imaginary
    }
    new ComplexArray(data)
  }

  /** Concatenate `rows` to a single ComplexArray. */
  def concatenate(rowData: Array[ComplexArray]): ComplexArray = {
    val rows = rowData.length
    val columns = rowData(0).length
    val newData = new Array[Float](2 * rows * rowData(0).length)
    val newDataLen = newData.length / 2
    for (row <- 0 until rows) {
      val offset = row * columns
      // real part copy
      Array.copy(rowData(row).data, 0, newData, offset, columns)
      // imaginary part copy
      Array.copy(rowData(row).data, columns, newData, newDataLen + offset, columns)
    }
    new ComplexArray(newData)
  }

  /** Required to implement IndexedSeq. XXX */
  val canBuildFrom = new CanBuildFrom[ComplexArray, Complex, ComplexArray] {
    def apply(from: ComplexArray) = new ComplexArrayBuilder
    def apply() = new ComplexArrayBuilder
  }

  /** Required to implement IndexedSeq  XXX */
  class ComplexArrayBuilder extends Builder[Complex, ComplexArray] {
    private var buffer = new ArrayBuffer[Complex]
    def +=(element: Complex) = {
      // Previous code made a copy of element's data, so we preserve the semantics.
      buffer += Complex(element.real, element.imaginary)
      this
    }
    def clear() {buffer = new ArrayBuffer[Complex]}
    def result() = new ComplexArray(buffer.toArray)
  }
}
