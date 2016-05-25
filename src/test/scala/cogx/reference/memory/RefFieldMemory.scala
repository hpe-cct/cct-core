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
import cogx.platform.types.ElementTypes.Float32
import cogx.cogmath.algebra.real.{Matrix, Scalar, Tensor, Vector}
import cogx.cogmath.geometry.Shape

/** Memory for representing real tensor fields. This hides the indexing and
  * padding of the internal representation.
  *
  * @param fieldType The type of the field (field shape, tensor shape, element
  *        type).
  * @param initData The data containing the field's state. If this is not specified,
  *        it will be initialized with all zeroes.
  *
  * @author Greg Snider
  */
class RefFieldMemory(val fieldType: FieldType, initData: Array[Float] = null)
        extends RefMemoryLayout
        with Iterable[Tensor]
{
  require(fieldType.elementType == Float32)
  require(if (initData != null) initData.length == arraySize else true,
    "Incorrect initData length (" + initData.length + "), expecting " + arraySize)

  /** Data held in field. */
  private[memory] val data = if (initData != null)
    initData
  else
    new Array[Float](arraySize)

  /** Combine two field memories on a point-by-point basis.
    *
    * @param that The second field memory to combine with this.
    * @param f Function that maps each element of this and that to a new value.
    * @return RefFieldMemory which is a point-wise combination of this and that.
    */
  def combine(that: RefFieldMemory, f: (Float, Float) => Float): RefFieldMemory = {
    require(this.fieldType == that.fieldType)
    val resultData = new Array[Float](data.length)
    for (i <- 0 until data.length)
      resultData(i) = f(this.data(i), that.data(i))
    new RefFieldMemory(fieldType, resultData)
  }

  /** Map a field memory on a point-by-point basis.
    *
    * @param f Function that maps each element of this to a new value.
    * @return RefFieldMemory which is a mapped version of this
    */
  def mapFloat(f: (Float) => Float): RefFieldMemory = {
    val resultData = new Array[Float](data.length)
    for (i <- 0 until data.length)
      resultData(i) = f(this.data(i))
    new RefFieldMemory(fieldType, resultData)
  }

  /** Return an iterator over all elements in the RealBuffer as a sequence of
    * Floats, skipping over internal padding.
    */
  def asRealSequence = new Iterator[Float] {
    private val columns =
      if (fieldType.dimensions == 0) 1
      else fieldType.fieldShape(fieldType.dimensions-1)
    private val columnPadding =
      if (fieldType.dimensions <= 1) 0
      else (fieldRowStride - columns)
    private var rowEndIndex = columns
    private var physicalIndex = 0
    private val maxPhysicalIndex =
      if (fieldType.dimensions <= 1) (columns-1)
      else (paddedFieldPoints-1)
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

  /** Returns an iterator over all elements of the RealBuffer. The state kept by the iterator
    * is a bit complicated due to optimizing for minimal processing in next() */
  def iterator = new Iterator[Tensor] {
    private val columns =
      if (fieldType.dimensions == 0) 1
      else fieldType.fieldShape(fieldType.dimensions-1)
    private val columnPadding =
      if (fieldType.dimensions <= 1) 0
      else (fieldRowStride - columns)
    private var rowEndIndex = columns
    private var physicalIndex = 0
    private var maxPhysicalIndex =
      if (fieldType.dimensions <= 1) (columns-1)
      else (paddedFieldPoints-1)
    def hasNext = physicalIndex <= maxPhysicalIndex
    def next = {
      val nextElement = readPhysical(physicalIndex)
      physicalIndex += 1
      if (physicalIndex >= rowEndIndex) {
        physicalIndex += columnPadding
        rowEndIndex = physicalIndex + columns
      }
      nextElement
    }
  }

  private[memory] def startIndexIterator = new Iterator[Int] {
    private val columns =
      if (fieldType.dimensions == 0) 1
      else fieldType.fieldShape(fieldType.dimensions-1)
    private val columnPadding =
      if (fieldType.dimensions <= 1) 0
      else (fieldRowStride - columns)
    private var rowEndIndex = columns
    private var physicalIndex = 0
    private var maxPhysicalIndex =
      if (fieldType.dimensions <= 1) (columns-1)
      else (paddedFieldPoints-1)
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
  def copy: RefFieldMemory = {
    val dataCopy = new Array[Float](data.length)
    Array.copy(data, 0, dataCopy, 0, data.length)
    new RefFieldMemory(fieldType, dataCopy)
  }

  /////////////////////////////////////////////////////////////////////////////
  //  Tensor accessors
  /////////////////////////////////////////////////////////////////////////////

  /** Private helper function to access a tensor in the buffer based on the physical
    * (i.e. buffer-padding-aware) index of the first element of the Tensor.  This function
    * is meant to be used by read() and iterators */
  private[RefFieldMemory] def readPhysical(physicalStartIndex: Int): Tensor = {
    var elementIndex = physicalStartIndex
    if (orderOfTensor == 0)
      new Scalar(data(elementIndex))
    else {
      val array = new Array[Float](tensorNumbers)
      for (i <- 0 until tensorNumbers) {
        array(i) = data(elementIndex)
        elementIndex += tensorStride
      }
      orderOfTensor match {
        case 1 => new Vector(array)
        case 2 => new Matrix(tensorShape(0), tensorShape(1), array)
        case _ =>
          throw new RuntimeException("Real tensors with order > 2 not done yet.")
      }
    }
  }

  /** Read the tensor at location specified by logical indices.
    * Specializations to the variable-arg version put in for performance.  -RJC */
  def read(index: Int): Tensor = readPhysical(physicalOffset(index))
  def read(rowIndex: Int, columnIndex: Int): Tensor = readPhysical(physicalOffset(rowIndex, columnIndex))
  def read(index: Int*): Tensor = readPhysical(physicalOffset(index: _*))

  /** Reading a Float out of a ScalarField optimized.  -RJC */
  def readFloat(index: Int): Float = {
    require(orderOfTensor == 0)
    data(physicalOffset(index))
  }
  def readFloat(rowIndex: Int, columnIndex: Int): Float = {
    require(orderOfTensor == 0)
    data(physicalOffset(rowIndex, columnIndex))
  }
  def readFloat(index: Int*): Float = {
    require(orderOfTensor == 0)
    data(physicalOffset(index: _*))
  }

  /** Write "tensor" to location specified by logical indices */
  def write(tensor: Tensor, index: Int*) {
    //printf("RefFieldMemory.write(")
    //index.foreach(i => printf(" %d", i))
    //printf(") = ")
    //printf("%f ", tensor.read(0))

    writePhysical(tensor, physicalOffset(index: _*))

    //if (fieldType.tensorShape.dimensions == 0)
    //  printf(" read: %f", readFloat(index: _*))
    //println(" physicalOffset: " + physicalOffset(index: _*))
  }

  /** Write "tensor" to location "index". */
  private[RefFieldMemory] def writePhysical(tensor: Tensor, physicalStartIndex: Int) {
    var elementIndex = physicalStartIndex
    require(tensor.length == tensorNumbers)
    for (i <- 0 until tensorNumbers) {
      data(elementIndex) = tensor.read(i)
      elementIndex += tensorStride
    }
  }

  /** Convert this Buffer to one used by ScalarFields.  "map" was already being used
    * to convert RealBuffers to other non-RealBuffer objects. */
  def toRealBuffer(f: Tensor => Float): RefFieldMemory = {
    val newBufferType = new FieldType(fieldShape, Shape(), Float32)
    val newBuffer = new RefFieldMemory(newBufferType)
    val newData = newBuffer.asRawArray
    val writeIndexer = newBuffer.startIndexIterator
    val reader = iterator
    while (reader.hasNext)
      newData(writeIndexer.next) = f(reader.next)
    newBuffer
  }

  /////////////////////////////////////////////////////////////////////////////
  //  Size and Shape manipulations
  /////////////////////////////////////////////////////////////////////////////

  /** Extract a subfield specified by index "ranges", resulting in a
    * smaller field with the same field dimension and tensor type.
    */
  def subfield(ranges: Range*): RefFieldMemory = {
    val dimensions = this.fieldType.fieldShape.dimensions
    require(ranges.toArray.length == dimensions && dimensions > 0)
    ranges.toArray.foreach((r: Range) => require(r.step == 1))

    val resultSize: Array[Int] = ranges.toArray.map(_.length)

    val resultShape = Shape(resultSize : _*)
    val resultFieldType = new FieldType(resultShape, fieldType.tensorShape, fieldType.elementType)
    val resultMem = new RefFieldMemory(resultFieldType)
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

//  def realTensorType(tensorShape: Shape) = tensorShape.dimensions match {
//    case 0 => ScalarTensor
//    case 1 => VectorTensor
//    case 2 => MatrixTensor
//    case _ => throw new RuntimeException("Not supported yet.")
//  }

  /**
   * downsample the field by taking every nth tensor in each dimension.
   */
  def downsample(factor: Int = 2, phase: Int = 0): RefFieldMemory = {
    val newFieldShape = fieldShape.map(dim => (dim - phase + (factor - 1)) / factor)
    val elementType = Float32
    val newFieldType = new FieldType(newFieldShape, tensorShape, elementType)
    val downsampled = new RefFieldMemory(newFieldType)
    for (indices <- newFieldShape.indices) {
      val fromIndices = indices.map(_ * factor + phase)
      val fromTensor = this.read(fromIndices : _*)
      downsampled.write(fromTensor, indices : _*)
    }
    downsampled
  }

  /**
   * upsample the field, writing to every nth tensor in each dimension.
   */
  def upsample(factor: Int = 2, phase: Int = 0): RefFieldMemory = {
    val newFieldShape = fieldShape.map(dim => dim * factor)
    val elementType = Float32
    val newFieldType = new FieldType(newFieldShape, tensorShape, elementType)
    val upsampled = new RefFieldMemory(newFieldType)
    for (indices <- fieldShape.indices) {
      val toIndices = indices.map(_ * factor + phase)
      val fromTensor = this.read(indices : _*)
      upsampled.write(fromTensor, toIndices : _*)
    }
    upsampled
  }

  /** Slice out a subfield along a first "index", producing a smaller field of
    * one smaller dimensions.
    */
  def slice(index: Int): RefFieldMemory = {
    require(fieldShape.dimensions > 0)
    val newFieldType = new FieldType(fieldShape.drop(1), tensorShape,
      Float32)
    val result = new RefFieldMemory(newFieldType)
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
  def stack(other: RefFieldMemory*): RefFieldMemory = {
    require(other.toArray.length > 0)
    val otherArray: Array[RefFieldMemory] = other.toArray
    for (field <- otherArray)
      require(field.fieldShape == this.fieldShape)
    val resultDimension = 1 + otherArray.length
    val resultFieldShape =
      Shape(Array.concat(Array(resultDimension), this.fieldShape.toArray) : _*)
    val resultFieldType =
      new FieldType(resultFieldShape, tensorShape, Float32)
    val result = new RefFieldMemory(resultFieldType)
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

  /** Extract the component indexed by "tensorIndex" in each tensor in the
    * field, returning a scalar field with the extracted components.
    *
    * The definition of sliceTensor here does not match that in the
    * CPUTensorSliceKernel.  This definition always produces a ScalarField
    * comprising one element from each tensor.  The CPUTensorSliceKernel
    * expects a single argument (an index) and drops one dimension from the
    * tensor.   XXX -RJC
    */
  def sliceTensor(tensorIndices: Int*): RefFieldMemory = {
    val pageIndex = tensorPageIndex(tensorIndices : _*)
    val resultType = new FieldType(fieldShape, Shape(), Float32)
    val indexIterator = startIndexIterator
    val offset = pageIndex * tensorStride
    val readData = data
    new RefFieldMemory(resultType) {
      while (indexIterator.hasNext) {
        val index = indexIterator.next
        data(index) = readData(index + offset)
      }
    }
  }

  /** Stack "this" field with 1 or more "other" fields to create a Vector field
    * with the same shape as this. All fields must have exactly the same
    * shape for their tensors to be stackable. This will only work for scalar
    * fields.
    */
  def stackTensor(other: RefFieldMemory*): RefFieldMemory = {
    require(tensorShape.dimensions == 0,
      "Only scalar fields may have their tensors stacked.")
    require(other.toArray.length > 0)
    for (buffer <- other.toArray)
      require(buffer.fieldType == this.fieldType)
    val buffers = Array.concat(Array(this), other.toArray)
    val vectorShape = Shape(buffers.length)
    val resultType = new FieldType(fieldShape, vectorShape, Float32)
    val result = new RefFieldMemory(resultType)
    for (indices <- fieldShape.indices) {
      val vector = new Vector(buffers.length) {
        for (i <- 0 until length)
          this(i) = buffers(i).read(indices : _*).read(0)
      }
      result.write(vector, indices : _*)
    }
    result
  }

  /** Get the shape of the field. */
  private def fieldShape = fieldType.fieldShape

  /** Get the shape of the tensors in the field. */
  private def tensorShape = fieldType.tensorShape

  /////////////////////////////////////////////////////////////////////////////
  //  Comparison
  /////////////////////////////////////////////////////////////////////////////

  /** Test "this" and "that" for approximate equality. Note that this skips
    * over the padded values within the field memory.
    */
  def ~==(that: RefFieldMemory): Boolean = {
    if (!(this.fieldType == that.fieldType))
      false
    else {
      val thisIter = this.iterator
      val thatIter = that.iterator
      while(thisIter.hasNext && thatIter.hasNext) {
        if (!approxEqual(thisIter.next, thatIter.next))
          return false
      }
      // Both iterators should be exhausted at this point.
      if (thisIter.hasNext || thatIter.hasNext)
        return false
      true
    }
    // Old form of the above 'else' clause.  Beautiful scala, but it never returned when
    // invoked on a 10000 x 10000 field comparison.
//      (this zip that).map(v =>approxEqual(v._1, v._2)).reduceLeft(_ && _)
  }

  /** Compare "a" and "b" for approximate equality. */
  private def approxEqual(a: Tensor, b: Tensor): Boolean = {
    for (i <- 0 until tensorNumbers) {
      // TODO: pick a new default threshold value for ~==
      // TODO: allow user-supplied thresholds for ~==
      //
      // I'm temporarily setting this to three significant digits, which is well
      // above the usual cut-off for approximate floating-point equality. It's
      // an easy default for testing, however, as three significant digits are
      // still relatively compact to display.
      //
      // A more robust solution will likely require support for both error norms
      // and user-supplied thresholds.
      //
      // -Ben
      //if (!(a.read(i) ~== b.read(i)))

      // I ran into more problems with comparing the reduced sum of some
      // moderately sized fields.  The resulting sums of random numbers were
      // > 1000, but the cumulative error made the first check fail.
      val aVal = a.read(i)
      val bVal = b.read(i)

      if (math.abs(aVal - bVal) > 0.001f) {
        // Perhaps a and b are close enough in a relative-error sense?
        if ((bVal == 0) || math.abs(aVal/bVal - 1f) > 1e-5)  {
          //          println("Value mismatch: a = " + aVal + ", b = " + bVal)
          return false
        }
      }
    }

    true
  }

  /** Test "this" and "other" for deep equality. Allows "==" to work. Note that
    * this skips over the padded values within the field memory.
    */
  override def equals(other: Any): Boolean = {
    // Old functional approach was inefficient in time and heap space:
    //          (this zip that).map(v => v._1 == v._2).reduceLeft(_ && _)
    other match {
      case that: RefFieldMemory =>
        if (this eq that)
          return true
        else if (fieldType != that.fieldType)
          return false
        else {
          val thatIterator = that.iterator
          val thisIterator = iterator
          while (thisIterator.hasNext && thatIterator.hasNext)
            if (thisIterator.next != thatIterator.next)
              return false
          require(!thisIterator.hasNext && !thatIterator.hasNext)
          return true
        }
      case _ => return false
    }
  }

  override def hashCode: Int = {
    arraySize
  }

  def asRawArray: Array[Float] = data


  /** This was put in to speed up CPUScalarReduceKernel back when apply() was slow.  Sadly,
    * after removing apply() in favor of iterators, using these iterators is still quite a
    * bit slower than the following approach to returning the unpadded array.
    */
  private[cogx] def asRawUnpaddedArray: Array[Float] = {
    val dataCopy = new Array[Float](fieldPoints*tensorNumbers)
    val columns = if (fieldType.dimensions == 0) 1 else fieldType.fieldShape(fieldType.dimensions-1)
    val rows = fieldPoints*tensorNumbers/columns
    for (row <- 0 until rows)
      Array.copy(data, row*fieldRowStride, dataCopy, row*columns, columns)
    dataCopy
  }

  private[cogx] def asPixelArray: Array[Int] =
    throw new RuntimeException("Illegal access of real array.")
}

/** Factory for creating RealBuffers from arrays of tensors.
  */
object RefFieldMemory {
  private lazy val random = new java.util.Random

  /** Create a memory filled with random numbers.
    */
  def random(fieldType: FieldType): RefFieldMemory = {
    val memory = new RefFieldMemory(fieldType)
    val data = memory.data
    for (i <- 0 until data.length)
      data(i) = random.nextFloat
    memory
  }

  /** Create a buffer of type "fieldType" from an array of tensors presented
    * in the order in which indices are traversed (as defined in the Shape
    * class).
    */
  def apply[T <: Tensor](fieldType: FieldType,
                         tensors: Iterable[T]): RefFieldMemory = {
    //require(fieldType.isReal)
    new RefFieldMemory(fieldType) {
      val writeIndexIterator = startIndexIterator
      val tensorIterator = tensors.iterator
      while (tensorIterator.hasNext && writeIndexIterator.hasNext)
        writePhysical(tensorIterator.next, writeIndexIterator.next)
      require(!tensorIterator.hasNext && !writeIndexIterator.hasNext)
    }
  }

  /** Create a buffer of type "fieldType" from an array of scalars presented
    * in the order in which indices are traversed (as defined in the Shape
    * class).
    */
  def fromFloats(fieldType: FieldType, scalars: Iterable[Float]): RefFieldMemory = {
    //require(fieldType.isReal)
    require(fieldType.tensorShape.dimensions == 0)
    new RefFieldMemory(fieldType) {
      val writeIndexIterator = startIndexIterator
      val tensorIterator = scalars.iterator
      while (tensorIterator.hasNext && writeIndexIterator.hasNext)
        data(writeIndexIterator.next) = tensorIterator.next
      require(!tensorIterator.hasNext && !writeIndexIterator.hasNext)
    }
  }

  /** Create a buffer of type "fieldType" from an array of Floats presented
    * in the order in which indices are traversed (as defined in the Shape
    * class).
    */
  def fromFloats(fieldType: FieldType, floats: Array[Float]): RefFieldMemory = {
    //require(fieldType.isReal)
    require(fieldType.tensorShape.dimensions == 0)

    new RefFieldMemory(fieldType) {
      require(floats.length == fieldPoints*tensorNumbers)
      // This will break for bizarre layouts, but generally, if the padded
      // length == the supplied length, then we assume a simple copy of the
      // data will suffice.
      if (floats.length == data.length) {
        Array.copy(floats, 0, data, 0, data.length)
      }
      else {
        val columns = if (fieldType.dimensions == 0) 1 else fieldType.fieldShape(fieldType.dimensions-1)
        val rows = fieldPoints*tensorNumbers/columns
        for (row <- 0 until rows)
          Array.copy(floats, row*columns, data, row*fieldRowStride, columns)
      }
    }
  }

  /** Create a buffer for an initialized 0D scalar field
    *
    * @param f Function which returns a scalar value for the given coordinates
    *          in the field.
    */
  def scalar(value: Float): RefFieldMemory = {
    val fieldType = new FieldType(Shape(), Shape(), Float32)
    val layout = RefMemoryLayout(fieldType)
    val buffer = new Array[Float](layout.arraySize)
    buffer(0) = value
    new RefFieldMemory(fieldType, buffer)
  }

  /** Create a buffer for an initialized 1D scalar field.
    *
    * @param columns Number of columns in field.
    * @param f Function which returns a scalar value for the given coordinates
    *          in the field.
    */
  def scalar(columns: Int)(f: (Int) => Float): RefFieldMemory = {
    val fieldType = new FieldType(Shape(columns), Shape(), Float32)
    val layout = RefMemoryLayout(fieldType)
    val buffer = new Array[Float](layout.arraySize)
    for (col <- 0 until columns) {
      val index = col
      buffer(index) = f(col)
    }
    new RefFieldMemory(fieldType, buffer)
  }

  /** Create a buffer for an initialized 2D scalar field.
    *
    * @param rows Number of rows in field.
    * @param columns Number of columns in field.
    * @param f Function which returns a scalar value for the given coordinates
    *          in the field.
    */
  def scalar(rows: Int, columns: Int)(f: (Int, Int) => Float): RefFieldMemory = {
    val fieldType = new FieldType(Shape(rows, columns), Shape(), Float32)
    val layout = RefMemoryLayout(fieldType)
    val paddedColumns = layout.fieldRowStride
    val buffer = new Array[Float](layout.arraySize)
    for (row <- (0 until rows).par) {
      for (col <- 0 until columns) {
        val index = row * paddedColumns + col
        buffer(index) = f(row, col)
      }
    }
    new RefFieldMemory(fieldType, buffer)
  }

  /** Create a buffer for an initialized 3D scalar field.
    *
    * @param rows Number of rows in field.
    * @param columns Number of columns in field.
    * @param f Function which returns a scalar value for the given coordinates
    *          in the field.
    */
  def scalar(layers: Int, rows: Int, columns: Int)(f: (Int, Int, Int) => Float): RefFieldMemory = {
    val fieldType = new FieldType(Shape(layers, rows, columns), Shape(), Float32)
    val layout = RefMemoryLayout(fieldType)
    val paddedColumns = layout.fieldRowStride
    val buffer = new Array[Float](layout.arraySize)
    for (layer <- (0 until layers).par) {
      for (row <- 0 until rows) {
        for (col <- 0 until columns) {
          val index = layer * rows * paddedColumns + row * paddedColumns + col
          buffer(index) = f(layer, row, col)
        }
      }
    }
    new RefFieldMemory(fieldType, buffer)
  }
}
