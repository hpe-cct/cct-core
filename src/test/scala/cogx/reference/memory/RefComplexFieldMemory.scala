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

import cogx.platform.types.FieldType
import cogx.platform.types.ElementTypes.Complex32
import cogx.cogmath.algebra.complex.{ComplexImplicits, ComplexArray, Complex}
import cogx.cogmath.geometry.Shape

/** Memory for representing complex tensor fields. This hides the indexing and
  * padding of the internal representation.
  *
  * @param fieldType The type of the field (field shape, tensor shape, element
  *        type).
  * @param initData The data containing the field's state. If this is not specified,
  *        it will be initialized with all zeroes.
  *
  * @author Greg Snider
  */
class RefComplexFieldMemory(val fieldType: FieldType, initData: Array[Float] = null)
        extends RefMemoryLayout
        with Iterable[Complex]
        with ComplexImplicits
{
  require(fieldType.elementType == Complex32)
  // Complex data requires 2 Floats per element.
  require(if (initData != null) initData.length == 2 * arraySize else true)

  /** Create a complex buffer from a ComplexArray. */
  def this(fieldType: FieldType, data: ComplexArray) =
    this(fieldType, data.asRawArray)

  /** Data held in field. */
  private[RefComplexFieldMemory] val data = if (initData != null)
    new ComplexArray(initData)
  else
    new ComplexArray(arraySize)

  /** Returns an iterator over all elements of the ComplexBuffer. The state kept by the iterator
    * is a bit complicated due to optimizing for minimal processing in next() */
  def iterator = new Iterator[Complex] {
    private val columns = if (fieldType.dimensions == 0) 1 else fieldType.fieldShape(fieldType.dimensions-1)
    private val columnPadding = if (fieldType.dimensions <= 1) 0 else (fieldRowStride - columns)
    private var rowEndIndex = columns
    private var physicalIndex = 0
    private var maxPhysicalIndex = if (fieldType.dimensions <= 1) (columns-1) else (paddedFieldPoints-1)
    def hasNext = physicalIndex <= maxPhysicalIndex
    def next = {
      val nextElement = data(physicalIndex)
      physicalIndex += 1
      if (physicalIndex >= rowEndIndex) {
        physicalIndex += columnPadding
        rowEndIndex = physicalIndex + columns
      }
      nextElement
    }
  }

  /** Returns an iterator over all elements of the ComplexBuffer. The state kept by the iterator
    * is a bit complicated due to optimizing for minimal processing in next() */
  def indexIterator = new Iterator[Int] {
    private val columns = if (fieldType.dimensions == 0) 1 else fieldType.fieldShape(fieldType.dimensions-1)
    private val columnPadding = if (fieldType.dimensions <= 1) 0 else (fieldRowStride - columns)
    private var rowEndIndex = columns
    private var physicalIndex = 0
    private var maxPhysicalIndex = if (fieldType.dimensions <= 1) (columns-1) else (paddedFieldPoints-1)
    def hasNext = physicalIndex <= maxPhysicalIndex
    def next = {
      val nextElement = physicalIndex
      physicalIndex += 1
      if (physicalIndex >= rowEndIndex) {
        physicalIndex += columnPadding
        rowEndIndex = physicalIndex + columns
      }
      nextElement
    }
  }

  /** Number of tensors in the buffer. */
  def length = fieldPoints

  /** Make a copy of "this". */
  def copy = new RefComplexFieldMemory(fieldType, data.copy.asRawArray)

  /////////////////////////////////////////////////////////////////////////////
  //  Tensor accessors
  /////////////////////////////////////////////////////////////////////////////

  /** Read the tensor at location specified by logical indices. */
  def read(index: Int*): Complex = data(physicalOffset(index: _*))

  /** Write "tensor" to location specified by logical indices. */
  def write(tensor: Complex, index: Int*) {
    //require(tensor.length == 1)
    data(physicalOffset(index : _*)) = tensor//.read(0)
  }

  /////////////////////////////////////////////////////////////////////////////
  //  Size and Shape manipulations
  /////////////////////////////////////////////////////////////////////////////

  /** Extract a subfield specified by index "ranges", resulting in a
    * smaller field with the same field dimension and tensor type.
    */
  def subfield(ranges: Range*): RefComplexFieldMemory = {
    val dimensions = this.fieldType.fieldShape.dimensions
    require(ranges.toArray.length == dimensions && dimensions > 0)
    ranges.toArray.foreach((r: Range) => require(r.step == 1))

    val resultSize: Array[Int] = ranges.toArray.map(_.length)

    val resultShape = Shape(resultSize : _*)
    val resultFieldType = new FieldType(resultShape, fieldType.tensorShape, fieldType.elementType)
    val resultMem = new RefComplexFieldMemory(resultFieldType)
    val offsets: Array[Int] = ranges.toArray.map(_.start)

    val toColumns = resultSize(resultSize.length-1)
    // When we finish copying a row, how much do the toIndex and fromIndex pointers
    // need to be bumped to get to the start of the next row, given the padding.
    val skippedToColumns = resultMem.fieldRowStride - toColumns
    val skippedFromColumns = fieldRowStride - toColumns

    val fromStartIndex = physicalOffset(offsets: _*)
    for (i <- 0 until tensorNumbers) {
      var fromIndex = fromStartIndex + i*tensorStride
      var toIndex = i*resultMem.tensorStride
      var toRowIndex = 0
      for (j <- 0 until resultMem.fieldPoints) {
        resultMem.data(toIndex) = data(fromIndex)
        toIndex += 1
        fromIndex += 1
        toRowIndex += 1
        if (toRowIndex == toColumns) {
          toRowIndex = 0
          toIndex += skippedToColumns
          fromIndex += skippedFromColumns
        }
      }
    }
    resultMem
  }

  /** Subsample the field by taking every other tensor in each dimension.
    */
  def downsample(factor:Int, phase:Int): RefComplexFieldMemory = {
    val newFieldShape = fieldShape.downsample(factor)
    val newFieldType = new FieldType(newFieldShape, tensorShape, Complex32)
    val downsampled = new RefComplexFieldMemory(newFieldType)
    for (indices <- newFieldShape.indices) {
      val fromIndices = indices.map(_ * factor + phase)
      val fromTensor = this.read(fromIndices : _*)
      downsampled.write(fromTensor, indices : _*)
    }
    downsampled
  }

  /** Slice out a subfield along a first "index", producing a smaller field of
    * one smaller dimensions.
    */
  def slice(index: Int): RefComplexFieldMemory = {
    require(fieldShape.dimensions > 0)
    val newFieldType = new FieldType(fieldShape.drop(1), Shape(), Complex32)
    val result = new RefComplexFieldMemory(newFieldType)
    for (indices <- newFieldType.fieldShape.indices) {
      val fromIndices = Array.concat(Array(index), indices)
      val value = read(fromIndices : _*)
      result.write(value, indices : _*)
    }
    result
  }

  /** Stack "this" field with 1 or more "other" fields to increase the
    * dimension of the result by 1. All fields must have exactly the same
    * shape to be stackable.
    */
  def stack(other: RefComplexFieldMemory*): RefComplexFieldMemory = {
    require(other.toArray.length > 0)
    val otherArray: Array[RefComplexFieldMemory] = other.toArray
    for (field <- otherArray)
      require(field.fieldShape == this.fieldShape)
    val resultDimension = 1 + otherArray.length
    val resultFieldShape =
      Shape(Array.concat(Array(resultDimension), this.fieldShape.toArray) : _*)
    val resultFieldType =
      new FieldType(resultFieldShape, Shape(), Complex32)
    val result = new RefComplexFieldMemory(resultFieldType)
    for (outerIndex <- 0 until resultDimension) {
      for (indices <- fieldShape.indices) {
        val readData = if (outerIndex == 0)
          this.read(indices : _*)
        else
          otherArray(outerIndex - 1).read(indices : _*)
        val toIndices = Array.concat(Array(outerIndex), indices)
        result.write(readData, toIndices : _*)
      }
    }
    result
  }

  /** Get the shape of the field. */
  def fieldShape = fieldType.fieldShape

  /** Get the shape of the tensors in the field. */
  def tensorShape = fieldType.tensorShape


  /////////////////////////////////////////////////////////////////////////////
  // Comparison
  /////////////////////////////////////////////////////////////////////////////

  /** Test "this" and "that" for approximate equality. Note that this skips
    * over the padded values within the field memory.
    */
  def ~==(that: RefComplexFieldMemory): Boolean = {
    if (!(this.fieldType == that.fieldType))
      false
    else
      (this zip that).map(v => approxEqual(v._1, v._2)).reduceLeft(_ && _)
  }

  /** Compare "a" and "b" for approximate equality. */
  private def approxEqual(a: Complex, b: Complex): Boolean = {
    if (!(a.real ~== b.real))
      return false
    if (!(a.imaginary ~== b.imaginary))
      return false
    true
  }


  /** Test "this" and "other" for deep equality. Allows "==" to work. Note that
    * this skips over the padded values within the field memory.
    */
  override def equals(other: Any): Boolean =
    other match {
      case that: RefComplexFieldMemory =>
        if (this.fieldType == that.fieldType)
          (this zip that).map(v => v._1 == v._2).reduceLeft(_ && _)
        else
          false
      case _ => false
    }

  override def hashCode: Int = {
    fieldType.fieldShape.points * fieldType.tensorShape.points
  }


  def asRawArray: Array[Float] = data.asRawArray

  def asPixelArray: Array[Int] =
    throw new RuntimeException("Illegal access of complex array.")
}

/** Factory for creating ComplexBuffers from arrays of tensors.
  */
object RefComplexFieldMemory {
  /** Create a buffer of type "fieldType" from an array of scalars presented
    * in the order in which indices are traversed (as defined in the Shape
    * class).
    */
  def apply(fieldType: FieldType, tensors: Array[Complex]): RefComplexFieldMemory = {
    require(fieldType.tensorOrder == 0 && fieldType.elementType == Complex32)
    val buffer = new RefComplexFieldMemory(fieldType)
    require(buffer.fieldPoints == tensors.length)
    var index = 0
    for (indices <- fieldType.fieldShape.indices) {
      buffer.write(tensors(index), indices : _*)
      index += 1
    }
    buffer
  }

  /** Create a buffer of type "fieldType" from an Iterable[Complex], which presents
    * Complex values in the order in which indices are traversed (as defined in the Shape
    * class).
    */
  def apply(fieldType: FieldType, complexes: Iterable[Complex]): RefComplexFieldMemory = {
    require(fieldType.elementType == Complex32)
    require(fieldType.tensorShape.dimensions == 0)
    new RefComplexFieldMemory(fieldType) {
      val writeIndexIterator = indexIterator
      val readIterator = complexes.iterator
      while (readIterator.hasNext && writeIndexIterator.hasNext)
        data(writeIndexIterator.next) = readIterator.next
      require(!readIterator.hasNext && !writeIndexIterator.hasNext)
    }
  }

  /** Create a buffer of type "fieldType" from an Iterable[Complex], which presents
    * Complex values in the order in which indices are traversed (as defined in the Shape
    * class).
    */
  def fromRealParts(fieldType: FieldType, realParts: Iterable[Float]): RefComplexFieldMemory = {
    require(fieldType.elementType == Complex32)
    require(fieldType.tensorShape.dimensions == 0)
    new RefComplexFieldMemory(fieldType) {
      val writeIndexIterator = indexIterator
      val readIterator = realParts.iterator
      while (readIterator.hasNext && writeIndexIterator.hasNext)
        data(writeIndexIterator.next) = readIterator.next
      require(!readIterator.hasNext && !writeIndexIterator.hasNext)
    }
  }
}
