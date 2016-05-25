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

import cogx.platform.types._
import cogx.platform.types.ElementTypes.Float32
import cogx.reference.memory.RefFieldMemory
import cogx.cogmath.algebra.real.{Tensor3, Tensor, Scalar, Matrix, Vector}
import cogx.cogmath.geometry.Shape
import cogx.utilities.Random

/** A field holding real scalars. This is the reference implementation, used
  * only for testing.
  *
  * @param _data The scalars held in the field.
  *
  * @author Greg Snider
  */
class RefScalarField private[cogx] (fieldType: FieldType, _data: => RefFieldMemory)
        extends RefField(fieldType)
{
  // Original constructor, before lazy instantiation was introduced
  private[cogx] def this(data: RefFieldMemory) = this(data.fieldType, data)

  // "lazy val" so as to not instantiate the data on the host node, if
  // a "matrixMaker" was supplied to the factory.
  lazy val data: RefFieldMemory = _data

  /** Create a RefScalarField with "shape", initialized with "data". */
  private[cogx] def this(shape: Shape, data: Array[Float]) =
    this(new RefFieldMemory(new FieldType(shape, Shape(), Float32), data))

  /** Read the scalar at location "indices". */
  def read(indices: Int*): Float = data.read(indices : _*).read(0)

  /** Iterate over the field in row-major order. */
  def iterator: Iterator[Float] = data.asRealSequence

  /** Make an exact copy of this field. */
  def copy = new RefScalarField(data.copy)

  // Scalar algebra:
  def +(that: Float) = new RefScalarField(data.mapFloat(_ + that))
  def -(that: Float) = new RefScalarField(data.mapFloat(_ - that))
  def *(that: Float) = new RefScalarField(data.mapFloat(_ * that))
  def /(that: Float) = new RefScalarField(data.mapFloat(_ / that))
  def %(that: Float) = new RefScalarField(data.mapFloat(_ % that))
  def unary_-() = this * -1f
  private[cogx] def reciprocal = new RefScalarField(data.mapFloat(1f / _))

  // Field algebra:
  def +(that: RefScalarField) = new RefScalarField(data.combine(that.data, _ + _))
  def -(that: RefScalarField) = new RefScalarField(data.combine(that.data, _ - _))
  def :*(that: RefScalarField) = new RefScalarField(data.combine(that.data, _ * _))
  def :/(that: RefScalarField) = new RefScalarField(data.combine(that.data, _ / _))
  def %(that: RefScalarField) = new RefScalarField(data.combine(that.data, _ % _))

  // Map, Reduce, Combine:
  def map(f: (Float) => Float): RefScalarField = {
    def ff(scalar: Tensor): Scalar = new Scalar(f(scalar.read(0)))
    new RefScalarField(RefFieldMemory(fieldType, data.map(ff)))
  }
  def reduce(f: (Float, Float) => Float): Float = {
    def ff(a: Tensor, b: Tensor): Tensor =
      new Scalar(f(a.read(0), b.read(0)))
    data.reduceLeft(ff).read(0)
  }
  def combine(that: RefScalarField, f: (Float, Float) => Float): RefScalarField =
    new RefScalarField(fieldType, this.data.combine(that.data, f))

  def subfield(ranges: Range*) =
    new RefScalarField(data.subfield(ranges : _*))

  def downsample(factor:Int = 2, phase:Int = 0) =
    new RefScalarField(data.downsample(factor, phase))

  def upsample(factor:Int = 2, phase:Int = 0) =
    new RefScalarField(data.upsample(factor, phase))

  /** Convert the field to a tensor. */
  def toTensor[Lat <: Tensor]: Lat = {
    fieldShape.dimensions match {
      case 0 => new Scalar(read()).asInstanceOf[Lat]
      case 1 => Vector(fieldShape(0), (i) => read(i)).asInstanceOf[Lat]
      case 2 => new Matrix(fieldShape(0), fieldShape(1),
        data.asRawUnpaddedArray).asInstanceOf[Lat]
      //        (r, c) => read(r, c)).asInstanceOf[Lat]

      /* I added this case to fix an exception thrown when trying to view 3D
       * RefScalarFields in the debugger. It seems like it could be added in an
       * OR clause to the above case, but I got ArrayIndexOutOfBounds exceptions
       * when I tried. The issue is that RealFieldPanel is trying to extract
       * layers from the field using the subfield method. Each layer is, for
       * all intents and purposes, 2D, but is described by a Shape with 3
       * dimensions - the last dimension just happens to be 1. So, without this
       * case, we fall into the default case down below, and can't view 3D
       * RefScalarFields.
       *
       * This isn't a general fix. A subfield operation could potentially
       * reduce any of the dimensions to 1, in which case the practical
       * dimensionality of the field could be 2D or lower. I'm not sure that it
       * matters to Cog anywhere but the GUI though. - TLG */
      case 3 if fieldShape(0) == 1 => new Matrix(fieldShape(1), fieldShape(2),
        data.asRawUnpaddedArray).asInstanceOf[Lat]

      case n: Int => throw new RuntimeException(
        "Cannot cast " + n + "-dimensional field to a Tensor.")
    }
  }

  /** Returns a field of all 0's with a single 1 in the location of "this"
    * that had the largest value in the field. If multiple values in "this"
    * are equal to max value, it returns a 1 in one of them. Only works
    * for scalar fields
    */
  def winnerTakeAll: RefScalarField = {
    val largestValue: Float = data.map(_.read(0)).reduceLeft(_ max _)
    val result = new RefFieldMemory(fieldType)
    for (index <- fieldShape.indices) {
      if (data.read(index : _*).read(0) == largestValue) {
        result.write(new Scalar(1f), index : _*)
        return new RefScalarField(result)
      }
    }
    throw new RuntimeException("winnerTakeAll internal error")
  }

  def dot(that: RefScalarField) = this :* that

  /** Given a 2D scalar field "this" and a matrix field "that", create a
    * tensor field with the same shape as that, but with the tensors at
    * every point in the result equal to "this" converted to a tensor.
    * This means that the shape of "this" must equal the shape of the tensor
    * points in "that".
    */
  def replicate(that: RefMatrixField): RefMatrixField = {
    require(dimensions == 2)
    require(this.fieldShape == that.tensorShape)
    val tensor = Matrix(fieldShape(0), fieldShape(1),
      (row, col) => this.read(row, col))
    val resultMemory = new RefFieldMemory(that.fieldType)
    for (indices <- that.fieldType.fieldShape.indices)
      resultMemory.write(tensor, indices : _*)
    new RefMatrixField(resultMemory)
  }

  /** Given a 1D scalar field "this" and a vector field "that", create a
    * tensor field with the same shape as that, but with the tensors at
    * every point in the result equal to "this" converted to a tensor.
    * This means that the shape of "this" must equal the shape of the tensor
    * points in "that".
    */
  def replicate(that: RefVectorField): RefVectorField = {
    require(dimensions == 1)
    require(this.fieldShape == that.tensorShape)
    val tensor = Vector(fieldShape(0), row => this.read(row))
    val resultMemory = new RefFieldMemory(that.fieldType)
    for (indices <- that.fieldType.fieldShape.indices)
      resultMemory.write(tensor, indices : _*)
    new RefVectorField(resultMemory)
  }

  /** Outer product of "this" and "that".
    *
    * If either field is 0 dimensional, it's
    * treated as a scalar multiplication of the other field.
    */
  def ^(that: RefScalarField): RefScalarField = {
    val resultShape = this.fieldShape concatenate that.fieldShape
    val resultType = new FieldType(resultShape, Shape(), Float32)
    val resultMemory = new RefFieldMemory(resultType)
    for (i <- this.fieldShape.indices) {
      val iValue = data.read(i : _*)
      for (j <- that.fieldShape.indices) {
        val jValue = that.data.read(j : _*)
        val resultIndex = Array.concat(i, j)
        resultMemory.write(new Scalar(iValue.read(0) * jValue.read(0)), resultIndex : _*)
      }
    }
    new RefScalarField(resultMemory)
  }

  /** Shift an N-D field by "distances", adding zeros to the
    * borders shifted away from.
    */
  def shift(distances: Int*): RefScalarField = {
    require(fieldShape.dimensions == distances.toArray.length)
    val resultMem = new RefFieldMemory(fieldType)

    def outOfBounds(indices: Array[Int]): Boolean = {
      val tooSmall = indices.map(_ < 0).reduceLeft(_ || _)
      val tooBig =
        (indices zip fieldShape.toArray).map(v => v._1 >= v._2).reduceLeft(_ || _)
      tooSmall || tooBig
    }

    // Add a + b, element by element
    def add(a: Array[Int], b: Array[Int]) = (a zip b).map(v => v._1 + v._2)

    for (indices <- fieldShape.indices) {
      val shiftedIndices: Array[Int] = add(indices.toArray, distances.toArray)
      if (!outOfBounds(shiftedIndices)) {
        val fromScalar = data.read(indices : _*)
        resultMem.write(fromScalar, shiftedIndices : _*)
      }
    }
    new RefScalarField(resultMem)
  }

  /** Shift an N-D field by "distances", wrapping the edges around in a
    * torus.
    */
  def shiftCyclic(distances: Int*): RefScalarField = {
    require(fieldShape.dimensions == distances.toArray.length)
    val resultMem = new RefFieldMemory(fieldType)

    def wrapIndices(indices: Array[Int]): Array[Int] = {
      val fieldSize = fieldShape.toArray
      val wrapped = new Array[Int](indices.length)
      for (i <- 0 until indices.length) {
        if (indices(i) < 0)
          wrapped(i) = indices(i) + fieldSize(i)
        else if (indices(i) >= fieldSize(i))
          wrapped(i) = indices(i) - fieldSize(i)
        else
          wrapped(i) = indices(i)
      }
      wrapped
    }

    // Add a + b, element by element
    def add(a: Array[Int], b: Array[Int]) = (a zip b).map(v => v._1 + v._2)

    for (indices <- fieldShape.indices) {
      val shiftedIndices: Array[Int] =
        wrapIndices(add(indices.toArray, distances.toArray))
      val fromScalar = data.read(indices : _*)
      resultMem.write(fromScalar, shiftedIndices : _*)
    }
    new RefScalarField(resultMem)
  }

  /** Extract all subfields of size "diameter". If "this" is 3-dimensional,
    * each subfield will be of size ("diameter" x "diameter" x "diameter"); if
    * "this" is 2-dimensional, it will be of size ("diameter" x "diameter").
    *
    * This creates field of the same dimension of "this",
    * but the tensors at each point in the field are replaced by the uniformly
    * spaced subfields expressed at tensors. For example, given a 3 x 4 x 5 field
    * of vectors of length 6, subfields(2) will produce a 3 x 4 x 5 field
    * of tensors of size (2 x 2 x 2 x 6). This can be viewed a preliminary
    * step towards inplementing a convolution where the kernel varies at every
    * point over the field.
    *
    * For subfields that extend beyond the border of "this", border-fill
    * extension is performed if "borderFill" is true, otherwise zero-fill is
    * done. Border fill will generally produce fewer edge distortions, but
    * zero-fill is even better if it is combined with normalized convolution.
    *
    * WARNING: currently only implemented for real, 2D scalar fields. XXX
    */
  def subfields(diameter: Int, border: BorderPolicy = BorderClamp): RefMatrixField = {
    require(diameter > 0 && diameter % 2 == 1, "Diameter must be odd.")
    require(dimensions == 2, "Subfields of 2D fields only.")
    val radius = diameter / 2
    val matrix = new Matrix(diameter, diameter)
    val resultType = new FieldType(border.convolutionShape(fieldShape, diameter),
      matrix.shape, Float32)
    val newMemory = new RefFieldMemory(resultType)

    for (row <- 0 until resultType.rows; col <- 0 until resultType.columns) {
      // Fill in the tensor at location (row, col)
      border match {
        case BorderClamp =>
          for (r <- 0 until diameter; c <- 0 until diameter)
            matrix(r, c) = readBorderFill(row + r - radius, col + c - radius)
          newMemory.write(matrix, row, col)
        case BorderZero =>
          for (r <- 0 until diameter; c <- 0 until diameter)
            matrix(r, c) = readZeroFill(row + r - radius, col + c - radius)
          newMemory.write(matrix, row, col)
        case BorderValid =>
          for (r <- 0 until diameter; c <- 0 until diameter)
            matrix(r, c) = readBorderFill(row + r, col + c)
          newMemory.write(matrix, row, col)
        case default =>
          throw new RuntimeException("unsupported border policy: " + border)
      }
    }
    def readBorderFill(row: Int, col: Int): Float = {
      val r = if (row < 0) 0 else if (row >= rows) rows - 1 else row
      val c = if (col < 0) 0 else if (col >= columns) columns - 1 else col
      data.readFloat(r, c)
    }
    def readZeroFill(row: Int, col: Int): Float = {
      if (row < 0 || row >= rows || col < 0 || col >= rows)
        0f
      else
        data.readFloat(row, col)
    }
    new RefMatrixField(newMemory)
  }

  /** Slice out a subfield along a first "index", producing a smaller field of
    * one smaller dimensions.
    */
  def slice(index: Int) =
    new RefScalarField(data.slice(index))

  /** Stack "this" field with 1 or more "other" fields to increase the
    * dimension of the result by 1. All fields must have exactly the same
    * shape to be stackable.
    */
  def stack(other: RefScalarField*) =
    new RefScalarField(data.stack(other.map(_.data) : _*))

  /** Stack "this" field with 1 or more "other" fields to create a Vector field
    * with the same shape as this. All fields must have exactly the same
    * shape for their tensors to be stackable.
    */
  def stackTensor(other: RefScalarField*) =
    new RefVectorField(data.stackTensor(other.map(_.data) : _*))

  /** Compare "this" and "that" for approximate equality. */
  def ~==(that: RefScalarField): Boolean = {
    data ~== that.data
  }

  // Value equality:
  override def equals(that: Any): Boolean = {
    that match {
      case other: RefScalarField => data == other.data
      case _ => false
    }
  }
  override def hashCode: Int = data.hashCode

  private[cogx] def asPixelArray = data.asPixelArray
  private[cogx] def asRawArray = data.asRawArray

  /** Print out a scalar field for debugging. */
  def print {
    fieldShape.dimensions match {
      case 0 =>
        println(read())
        println
      case 1 =>
        for (col <- 0 until fieldShape(0))
          printf(" %6.3f", read(col))
        println
      case 2 =>
        for (row <- 0 until fieldShape(0)) {
          for (col <- 0 until fieldShape(1)) {
            printf(" %6.3f", read(row, col))
          }
          println
        }
        println
      case 3 =>
        println("  RefScalarField.print " + fieldShape + " layers: " + layers +
                " rows: " + rows + " columns: " + columns)
        for (layer <- 0 until fieldShape(0)) {
          for (row <- 0 until fieldShape(1)) {
            for (col <- 0 until fieldShape(2))
              printf(" %8.3f", read(layer, row, col))
            println
          }
          println
        }
        println
      case x =>
        throw new RuntimeException("not implemented for " + x + "-D fields.")
    }
  }

  /** Generate a single-line string containing the field data, used in test suites. */
  def compactString: String = {
    val s = new StringBuilder("RefScalarField[")

    fieldShape.dimensions match {
      case 0 =>
        s ++= "](%.3f)".format(read())
      case 1 =>
        s ++= "%d](".format(fieldShape(0))
        for (col <- 0 until fieldShape(0)) {
          if (col < fieldShape(0)-1)
            s ++= "%.3f, ".format(read(col))
          else
            s ++= "%.3f".format(read(col))
        }
        s ++= ")"
      case 2 =>
        s ++= "%d, %d](".format(fieldShape(0), fieldShape(1))
        for (row <- 0 until fieldShape(0)) {
          s ++= "("

          for (col <- 0 until fieldShape(1)) {
            if (col < fieldShape(1)-1)
              s ++= "%.3f, ".format(read(row, col))
            else
              s ++= "%.3f".format(read(row, col))
          }

          if (row < rows-1)
            s ++= "), "
          else
            s ++= ")"
        }
        s ++= ")"
      case x =>
        throw new RuntimeException("not implemented for " + x + "-D fields.")
    }

    s.toString
  }

  /** Print out a scalar field for debugging. This uses hexadecimal format which
    * is useful for chasing down big-endian, little-endian formatting problems.
    */
  def printHex {
    fieldShape.dimensions match {
      case 0 =>
        println(read())
        println
      case 1 =>
        for (col <- 0 until fieldShape(0))
          printf(" %08x", java.lang.Float.floatToRawIntBits(read(col)))
        println
      case 2 =>
        for (row <- 0 until fieldShape(0)) {
          for (col <- 0 until fieldShape(1)) {
            printf(" %08x",  java.lang.Float.floatToRawIntBits(read(row, col)))
          }
          println
        }
        println
      case x =>
        throw new RuntimeException("not implemented for " + x + "-D fields.")
    }
  }
}

/** Factory for creating real scalar fields.
  *
  * @author Greg Snider
  */
object RefScalarField {
  private lazy val rand = new Random

  /** Create field memory type for shape "sizes". */
  private def createFieldType(sizes: Int*): FieldType = {
    val fieldShape = Shape(sizes : _*)
    val fieldType = new FieldType(fieldShape, Shape(), Float32)
    fieldType
  }

  /** Create field memory of shape "sizes". */
  private def createFieldMem(sizes: Int*): RefFieldMemory = {
    new RefFieldMemory(createFieldType(sizes: _*))
  }

  // Create a zero-filled ScalarFields of 1, 2, and 3 dimensions.
  def apply(columns: Int) =
    new RefScalarField(createFieldMem(columns))
  def apply(rows: Int, columns: Int) =
    new RefScalarField(createFieldMem(rows, columns))
  def apply(layers: Int, rows: Int, columns: Int) =
    new RefScalarField(createFieldMem(layers, rows, columns))
  def apply(shape: Shape) =
    new RefScalarField(createFieldMem(shape.toArray : _*))
  def apply(shape: Shape, unpaddedData: Array[Float]) =
    new RefScalarField(RefFieldMemory.fromFloats(createFieldType(shape.toArray : _*), unpaddedData))

  // Create a 0-dimensional ScalarField holding "value" at its only point. */
  def apply(value: Float): RefScalarField = {
    new RefScalarField(RefFieldMemory.scalar(value))
  }

  // Create ScalarField of 1, 2, 3 dimensions filled by "f"
  def apply(columns: Int, f: (Int) => Float): RefScalarField = {
    new RefScalarField(RefFieldMemory.scalar(columns)(f))
  }

  def apply(rows: Int, cols: Int, f: (Int, Int) => Float): RefScalarField = {
    new RefScalarField(RefFieldMemory.scalar(rows, cols)(f))
  }

  def apply(layers: Int, rows: Int, cols: Int, f: (Int, Int, Int) => Float): RefScalarField = {
    new RefScalarField(RefFieldMemory.scalar(layers, rows, cols)(f))
  }

  /** Create a random RefScalarField of shape "sizes". */
  def random(sizes: Int*): RefScalarField = {
    val fieldType = new FieldType(Shape(sizes :_*), Shape(), Float32)
    new RefScalarField(RefFieldMemory.random(fieldType))
  }

  /** Create a random RefScalarField of shape 'shape'. */
  def random(shape: Shape): RefScalarField =
    random(shape.toArray:_*)

  /** Create a 1D scalar field from a vector. */
  def apply(vector: Vector): RefScalarField = {
    val mem = createFieldMem(vector.length)
    for (i <- 0 until vector.length)
      mem.write(new Scalar(vector(i)), i)
    new RefScalarField(mem)
  }

  /** Create a 2D scalar field from a matrix. */
  def apply(matrix: Matrix): RefScalarField = {
    val mem = createFieldMem(matrix.rows, matrix.columns)
    for (row <- 0 until matrix.rows; col <- 0 until matrix.columns)
      mem.write(new Scalar(matrix(row, col)), row, col)
    new RefScalarField(mem)
  }

  /** Create a 3D scalar field from a matrix3D. */
  def apply(matrix: Tensor3): RefScalarField = {
    val slices = matrix.layers
    val rows = matrix.rows
    val columns = matrix.columns
    val mem = createFieldMem(slices, rows, columns)
    for (slice <- 0 until slices; row <- 0 until rows; col <- 0 until columns)
      mem.write(new Scalar(matrix(slice, row, col)), slice, row, col)
    new RefScalarField(mem)
  }

  /** Create a 2D scalar field from a function that returns a Matrix. */
  def apply(rows: Int, cols: Int, matrixMaker: () => Matrix): RefScalarField = {
    val fieldType = createFieldType(rows, cols)
    def realBufferMaker = {
      val matrix: Matrix = matrixMaker()
      new RefFieldMemory(fieldType) {
        for (row <- 0 until rows; col <- 0 until cols)
          write(new Scalar(matrix(row,col)), row, col)
      }
    }
    new RefScalarField(fieldType, realBufferMaker) {
      // The GPUEvaluator class currently uses a HashMap to quickly check
      // for operations it has already compiled, but this calls hashCode for
      // this class, which instantiates the potentially large data array on
      // the Host node.  To prevent this, we override the default
      // RefScalarField.hashCode that is based on the data, choosing instead
      // the simpler, though legitimate, hashCode based only on the fieldType.
      override def hashCode = fieldType.hashCode
    }
  }
}

object Dummy {
  def main(args: Array[String]) {
    val field = RefScalarField.random(5, 5)
    field.print
  }
}
