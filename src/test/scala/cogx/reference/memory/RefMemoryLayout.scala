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
import cogx.platform.types.ElementTypes.{Float32, Complex32}


/** Base class for all field buffers that are shared by the CPU and GPU. This
  * organizes the data within the buffer so that memory accesses by the GPU are
  * efficient.
  *
  * To make field operations efficient on the GPU, we allocate CPU memory
  * for fields that mimics their structure on the GPU. This provides a uniform
  * representation of fields that's consistent everywhere.
  *
  * Update: Padding of columns has been removed!  Motivation:
  *
  * 1. With caches now common in GPUs, this had no measurable performance benefit.
  * 2. The native runtime exposes the layout to the user apps, and no notion of
  *    padding was described.
  * 3. The padding introduced corner-cases where the footprint of certain fields
  *    grows dramatically, which affects buffer transfer times between GPUs and
  *    makes the GPU caches less effective.
  *
  * '''Scalar Fields'''
  *
  * Scalar fields are the easiest place to start. It looks like this:
  * {{{
  *
  *   2D Scalar field:
  *   +------------------------+
  *   |                        |
  *   |                        |
  *   |     Field (10 x 19)    |
  *   |                        |         <-- page
  *   |                        |
  *   +------------------------+
  *         Memory (10 x 19)
  *
  * }}}
  * The array is then flattened to a linear array by reading the rows in order
  * from top to bottom. In general, this means that last index into the field
  * varies the fastest in the linear array. This linear array is defined to
  * be a "page."
  *
  * '''Vector Fields'''
  *
  * Tensor fields with tensor order > 0 (e.g. vector fields, matrix fields)
  * are laid out as several consecutive "pages," where each page contains one
  * element of that tensor at each point in the field. For example, a 2D vector
  * field with length 2 vectors would look like this:
  * {{{
  *
  *   2D Vector field:
  *   +------------------------+
  *   |                        |
  *   |                        |
  *   |     Field (10 x 19)    |
  *   |                        |         <-- page (vector element 0)
  *   |                        |
  *   +------------------------+
  *         Memory (10 x 19)
  *
  *   +------------------------+
  *   |                        |
  *   |                        |
  *   |     Field (10 x 19)    |
  *   |                        |         <-- page (vector element 1)
  *   |                        |
  *   +------------------------+
  *         Memory (10 x 19)
  *
  * }}}
  *
  * '''Matrix Fields'''
  *
  * Matrix fields are laid out in row-major order:
  * {{{
  *
  *   2D matrix field:
  *   +------------------------+
  *   |                        |
  *   |                        |
  *   |     Field (10 x 19)    |
  *   |                        |         <-- page (matrix element 0, 0)
  *   |                        |
  *   +------------------------+
  *         Memory (10 x 19)
  *
  *   +------------------------+
  *   |                        |
  *   |                        |
  *   |     Field (10 x 19)    |
  *   |                        |         <-- page (matrix element 0, 1)
  *   |                        |
  *   +------------------------+
  *         Memory (10 x 19)
  *
  *   +------------------------+
  *   |                        |
  *   |                        |
  *   |     Field (10 x 19)    |
  *   |                        |         <-- page (matrix element 1, 0)
  *   |                        |
  *   +------------------------+
  *         Memory (10 x 19)
  *
  *   +------------------------+
  *   |                        |
  *   |                        |
  *   |     Field (10 x 19)    |
  *   |                        |         <-- page (matrix element 1, 1)
  *   |                        |
  *   +------------------------+
  *         Memory (10 x 19)
  **
  * }}}
  * The above approach applies to both real and complex fields.
  *
  * Color fields are done differently, because they are typically processed
  * using texture cache hardware using a proprietary buffer layout. In this
  * case, no padding of pages is necessary, so color fields are simply
  * represented as opaque memory:
  * {{{
  *
  *   2D Color field:
  *   +------------------------+
  *   |                        |
  *   |                        |
  *   |     Field (10 x 19)    |
  *   |     Memory (10 x 19)   |
  *   |                        |
  *   +------------------------+
  *
  * }}}
  * This memory layout wastes some memory, though not much for large fields.
  * It has the nice property that it makes memory coalescing much simpler on
  * the GPU, an important attribute for performance.
  *
  * @author Greg Snider
  */
trait RefMemoryLayout {
  /** The type of field being laid out. Because this is abstract, the remaining
    * fields in this trait that depend on it must be lazy.
    */
  val fieldType: FieldType

  /** Number of 'numbers' in each tensor. */
  lazy val tensorNumbers = fieldType.tensorShape.points

  /** Number of tensors in the field. */
  lazy val fieldPoints = fieldType.fieldShape.points

  /** Number of tensors in the field, including the tensors added for padding. */
  lazy val paddedFieldPoints = pageSize
  /** The size of a "page" of memory. Each page holds a field of scalars,
    * corresponding to a single element for each tensor in the field.
    * Image fields, regardless of whether they hold 1, 2, or 4 bytes per pixel,
    * are always only a single page.
    */
  lazy val pageSize: Int = {
    val fieldShape = fieldType.fieldShape
    fieldType.elementType match {
      case Complex32 =>
        // Round up the smallest dimension to a multiple of MemoryBlockSize.
        // 0-dimensional fields are represented by a single number.
        val array = fieldShape.toArray
        // Padding of rows to MemoryBlockSize has been removed.
//        if (array.length > 0)
//          array(array.length - 1) = roundUpSize(array(array.length - 1))
        array.foldLeft(1)(_ * _)
      /*
    case MonoImageTensor =>
      // No padding for images
      fieldShape.toArray.foldLeft(1)(_ * _)
    case BiImageTensor =>
      // No padding for images
      fieldShape.toArray.foldLeft(1)(_ * _)
    case QuadImageTensor =>
      // No padding for images
      fieldShape.toArray.foldLeft(1)(_ * _)
      */
      case Float32 =>
        // Round up the smallest dimension to a multiple of MemoryBlockSize.
        // 0-dimensional fields are represented by a single number.
        val array = fieldShape.toArray
        // Padding of rows to MemoryBlockSize has been removed.
//        if (array.length > 0)
//          array(array.length - 1) = roundUpSize(array(array.length - 1))
        array.foldLeft(1)(_ * _)
      case x =>
        throw new RuntimeException("not implemented yet: " + x)
    }
  }

  /** Number of elements (regardless of type) in the field buffer. A real
    * number, a complex number, or a pixel each count as one element. Note that
    * this includes internal "padding" necessary for efficient GPU access.
    */
  lazy val arraySize: Int = pageSize * tensorNumbers

  /** The number of words (32 bits) needed to store the field. */
  def wordsOfStorage = {
    fieldType.elementType match {
      //case ComplexTensor =>
      //  2 * arraySize
      case Float32 =>
        arraySize
      case x =>
        throw new RuntimeException("not implemented yet: " + x)
    }
  }


  /** The order of the tensors: 0 => scalar, 1 => vector, 2 => matrix, ...
    */
  private[reference] lazy val orderOfTensor = fieldType.tensorShape.dimensions

  /** Element distance between successive elements of a tensor within a field.
    * This is only used for fields with tensors of order > 0.
    */
  lazy val tensorStride: Int = pageSize

  /** Element distance between successive elements as each index is incremented.
    * The right-most index varies fastest so its stride is 1. The further left
    * the index, the larger the stride.
    */
  lazy val fieldStride: Array[Int] = {
    val array = new Array[Int](fieldType.fieldShape.dimensions)
    val fieldShape = fieldType.fieldShape
    var scale = 1
    for (i <- (array.length - 1) to 0 by -1) {
      array(i) = scale
      // Padding of rows to MemoryBlockSize has been removed.
//      // Real and complex fields
//      if (scale == 1)
//        scale *= roundUpSize(fieldShape(i))
//      else
//        scale *= fieldShape(i)
      scale *= fieldShape(i)
    }
    array
  }

  /** Fields are organized into "rows", where a row corresponds to the smallest
    * dimension of the field. The length of a row is equal to the size of
    * the smallest dimension rounded up to the next multiple of
    * MemoryBlockSize. This is needed by GPU kernels to separate the rows
    * at runtime.
    */
  lazy val fieldRowStride: Int = {
    if (fieldStride.length > 1)
      fieldStride(fieldStride.length - 2)
    else
      1
  }

  /** The logical "offset" of a particular tensor given its "location" as
    * an array of "index". This translates a logical address (array of indices)
    * to a sequential index.
    */
  private[reference] def logicalOffset(index: Int*): Int = {
    val indices = index.toArray
    require(indices.length == logicalFieldStride.length)
    val offset = (indices zip logicalFieldStride).map(v => v._1 * v._2).foldLeft(0)(_ + _)
    offset
  }

  /** The physical offset of the start of a particular tensor given its logical
    * location "index".
    */
  private[reference] def physicalOffset(index: Int): Int = {
    require(fieldStride.length == 1)
    index
  }

  private[reference] def physicalOffset(rowIndex: Int, columnIndex: Int): Int = {
    require(fieldStride.length == 2)
    rowIndex * fieldRowStride + columnIndex
  }

  private[reference] def physicalOffset(layerIndex: Int, rowIndex: Int, colIndex: Int): Int = {
    throw new RuntimeException("not done yet")
  }

  protected[cogx] def physicalOffset(index: Int*): Int = {
    val indices = index.toArray
    require(indices.length == fieldStride.length)
    val offset = (indices zip fieldStride).map(v => v._1 * v._2).foldLeft(0)(_ + _)
    offset
  }

  /** Field stride if field was packed compactly into a buffer. */
  private lazy val logicalFieldStride: Array[Int] = {
    val fieldShape = fieldType.fieldShape
    val sizes = fieldShape.toArray
    var scale = 1
    for (i <- (sizes.length - 1) to 0 by -1) {
      sizes(i) = scale
      scale *= fieldShape(i)
    }
    sizes
  }

  /** Picturing a tensor as a linear array (although it's not stored that
    *  way), what are the strides for the various component indices. */
  private lazy val logicalTensorStride: Array[Int] = {
    val tensorShape = fieldType.tensorShape
    val sizes = tensorShape.toArray
    var scale = 1
    for (i <- (sizes.length - 1) to 0 by -1) {
      sizes(i) = scale
      scale *= tensorShape(i)
    }
    sizes
  }

  /** A "page" holds all the tensor elements having the same tensor indices.
    * "tensorPageIndex" is the linear page index of the page corresponding to
    * a given set of tensor indices.
    */
  private[reference] def tensorPageIndex(index: Int*): Int = {
    val indices = index.toArray
    require(indices.length == logicalTensorStride.length,
      "actual: " + indices.length + ", expected: " + logicalTensorStride.length)
    val offset = (indices zip logicalTensorStride).map(v => v._1 * v._2).foldLeft(0)(_ + _)
    offset
  }

  /** The physical index of sequential tensor "index". */
  private[reference] def start(index: Int): Int = {
    // Convert the sequential index to a logical field index, then
    // compute the offset. This is a mixed-radix conversion from the
    // fieldShape radix representation to the fieldStride radix representation.
    val fieldShape = fieldType.fieldShape
    val logicalIndices =
      (logicalFieldStride.map(index / _) zip fieldShape.toArray).map(v => v._1 % v._2)
    //offset(logicalIndices : _*)
    (logicalIndices zip fieldStride).map(v => v._1 * v._2).foldLeft(0)(_ + _)
  }

  protected def indexOf(tensorIndex: Int, elementIndex: Int): Int =
    start(tensorIndex) + elementIndex * tensorStride

}

/** A factory for creating RefMemoryLayouts.
  */
object RefMemoryLayout {
  def apply(f: FieldType): RefMemoryLayout = new RefMemoryLayout {val fieldType = f}
}
