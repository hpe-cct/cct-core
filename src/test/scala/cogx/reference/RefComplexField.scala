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

import cogx.platform.types.FieldType
import cogx.platform.types.ElementTypes.{Float32, Complex32}
import cogx.reference.memory.{RefFieldMemory, RefComplexFieldMemory}
import cogx.cogmath.geometry.Shape
import cogx.cogmath.algebra.complex.{ComplexMatrix, ComplexVector, Complex}
import java.util.Random

/** A field holding complex scalars.
  *
  * @param _data The complex scalars held in the field.
  *
  * @author Greg Snider
  */
class RefComplexField private[cogx] (fieldType: FieldType, _data: => RefComplexFieldMemory)
        extends RefField(fieldType)
{
  // Original constructor, before lazy instantiation was introduced
  private[cogx] def this(data: RefComplexFieldMemory) = this(data.fieldType, data)

  // "lazy val" so as to not instantiate the data on the host node, if
  // a "matrixMaker" was supplied to the factory.
  lazy val data: RefComplexFieldMemory = _data

  /** Create a RefComplexField with "shape", initialized with "data". */
  private[cogx] def this(shape: Shape, data: Array[Float]) =
    this(new RefComplexFieldMemory(
      new FieldType(shape, Shape(), Complex32), data))

  /** Create a zero-filled RefComplexField with "shape". */
  private[cogx] def this(shape: Shape) =
    this(new RefComplexFieldMemory(
      new FieldType(shape, Shape(), Complex32)))

  /** Read the scalar at location "indices". */
  def read(indices: Int*): Complex = data.read(indices : _*).asInstanceOf[Complex]

  /** Make an exact copy of this field. */
  def copy = new RefComplexField(data.copy)

  // Subfield access:
  def subfield(ranges: Range*) =
    new RefComplexField(data.subfield(ranges : _*))

  def downsample(factor:Int, phase:Int) =
    new RefComplexField(data.downsample(factor, phase))

  /** Convert a 1D field to a complex vector. */
  def toComplexVector: ComplexVector = {
    ComplexVector(fieldShape(0), (i) => read(i))
  }

  /** Convert a 2D field to a complex matrix. */
  def toComplexMatrix: ComplexMatrix = {
    dimensions match {
      case 2 => ComplexMatrix(fieldShape(0), fieldShape(1), (r, c) => read(r, c))
      /* The 3D case was added for the GUI, which uses our existing display for
       * 2D complex fields to show 3D complex fields as a bunch of 2D slices. */
      case 3 if fieldShape(0) == 1 => ComplexMatrix(fieldShape(1), fieldShape(2), (r, c) => read(0, r, c))
      case _ => sys.error("ERROR: Can only convert 2D fields to matrices.")
    }
  }

  // Split real / imaginary
  def realPart = mapToReal(_.real)
  def imaginaryPart = mapToReal(_.imaginary)

  // ComplexScalar algebra:
  def +(that: Float) = map(_ + that)
  def -(that: Float) = map(_ - that)
  def *(that: Float) = map(_ * that)
  def /(that: Float) = map(_ / that)
  def unary_-() = map(_ * -1f)
  private[cogx] def reciprocal = map(_.reciprocal)

  def +(that: Complex) = map(_ + that)
  def -(that: Complex) = map(_ - that)
  def *(that: Complex) = map(_ * that)
  def /(that: Complex) = map(_ / that)

  // Field algebra:
  def +(that: RefComplexField) = combine(that, _ + _)
  def -(that: RefComplexField) = combine(that, _ - _)
  def :*(that: RefComplexField) = combine(that, _ * _)
  def :/(that: RefComplexField) = combine(that, _ / _)

  // Map, Reduce, Combine:
  def map(f: (Complex) => Complex): RefComplexField = {
    //def ff(scalar: Tensor[Complex]): Complex = f(scalar.asInstanceOf[Complex])
    new RefComplexField(RefComplexFieldMemory(fieldType, data.map(f).toArray))
  }
  def reduce(f: (Complex, Complex) => Complex): Complex = {
    //def ff(a: Tensor[Complex], b: Tensor[Complex]): Tensor[Complex] =
    //  f(a.asInstanceOf[Complex], b.asInstanceOf[Complex])
    //new Complex(data.reduceLeft(f))
    data.reduceLeft(f)
  }
  def combine(that: RefComplexField,
              f: (Complex, Complex) => Complex): RefComplexField = {
    new RefComplexField(RefComplexFieldMemory(fieldType,
      (data zip that.data).map(v => f(v._1.asInstanceOf[Complex], v._2.asInstanceOf[Complex])).toArray))
  }
  def mapToReal(f: (Complex) => Float): RefScalarField = {
    //def ff(scalar: Tensor[Complex]): Float = f(scalar.asInstanceOf[Complex])
    //new RefScalarField(FieldMemory.fromFloats(fieldType.toReal, data.map(ff)))
    val scalarFieldType = new FieldType(fieldType.fieldShape, fieldType.tensorShape, Float32)
    new RefScalarField(RefFieldMemory.fromFloats(scalarFieldType, data.map(f)))
  }

  def dot(that: RefComplexField) = this :* that

  /**
   * Slice out a subfield along a first "index", producing a smaller field of
   * one smaller dimensions.
   */
  def slice(index: Int) =
    new RefComplexField(data.slice(index))

  /**
   * Stack "this" field with 1 or more "other" fields to increase the
   * dimension of the result by 1. All fields must have exactly the same
   * shape to be stackable.
   */
  def stack(other: RefComplexField*) =
    new RefComplexField(data.stack(other.map(_.data) : _*))

  /** Compare "this" and "that" for approximate equality. */
  def ~==(that: RefComplexField): Boolean = {
    data ~== that.data
  }

  // Value equality:
  override def equals(that: Any): Boolean = {
    if (that.isInstanceOf[RefComplexField])
      data == that.asInstanceOf[RefComplexField].data
    else
      false
  }
  override def hashCode: Int = data.hashCode

  private[cogx] def asPixelArray = data.asPixelArray
  private[cogx] def asRawArray = data.asRawArray

  /** Print out a complex scalar field for debugging. */
  def print {
    println("ComplexRefScalarField")
    println("real part:")
    this.realPart.print
    println("imaginary part:")
    this.imaginaryPart.print
    println
  }

}

/** Factory for creating complex scalar fields.
  *
  * @author Greg Snider
  */
object RefComplexField {
  private lazy val rand = new Random

  /** Create field memory type for shape "sizes". */
  private def createFieldType(sizes: Int*): FieldType = {
    val fieldShape = Shape(sizes : _*)
    val fieldType = new FieldType(fieldShape, Shape(), Complex32)
    fieldType
  }

  /** Create field memory of shape "sizes". */
  private def createFieldMem(sizes: Int*): RefComplexFieldMemory = {
    new RefComplexFieldMemory(createFieldType(sizes: _*))
  }

  /** Create a complex scalar field from "real" and "imaginary" scalar fields.*/
  def apply(real: RefScalarField, imaginary: RefScalarField): RefComplexField = {
    require(real.fieldType == imaginary.fieldType)
    val resultType =
      new FieldType(real.fieldType.fieldShape, Shape(), Complex32)
    val resultMem = new RefComplexFieldMemory(resultType)
    for (indices <- real.fieldType.fieldShape.indices) {
      val realPart = real.data.read(indices : _*).read(0)
      val imaginaryPart = imaginary.data.read(indices : _*).read(0)
      resultMem.write(Complex(realPart, imaginaryPart), indices : _*)
    }
    new RefComplexField(resultMem)
  }

  /** Create a complex scalar field from a "real" scalar field.*/
  def apply(real: RefScalarField): RefComplexField = {
    val resultType =
      new FieldType(real.fieldType.fieldShape, Shape(), Complex32)
    val resultMem = new RefComplexFieldMemory(resultType)
    for (indices <- real.fieldType.fieldShape.indices) {
      val realPart = real.data.read(indices : _*).read(0)
      val imaginaryPart = 0f
      resultMem.write(Complex(realPart, imaginaryPart), indices : _*)
    }
    new RefComplexField(resultMem)
  }

  /**
   * Create a complex scalar field from "magnitude" and "phase" scalar fields.
   */
  def polar(magnitude: RefScalarField, phase: RefScalarField): RefComplexField = {
    require(magnitude.fieldType == phase.fieldType)
    val resultType =
      new FieldType(magnitude.fieldType.fieldShape, Shape(), Complex32)
    val resultMem = new RefComplexFieldMemory(resultType)
    for (indices <- magnitude.fieldType.fieldShape.indices) {
      val magnitudePart = magnitude.data.read(indices : _*).read(0)
      val phasePart = phase.data.read(indices : _*).read(0)
      resultMem.write(Complex.polar(magnitudePart, phasePart), indices : _*)
    }
    new RefComplexField(resultMem)
  }

  // Create a zero-filled RefScalarFields of 1, 2, and 3 dimensions.
  def apply(columns: Int) =
    new RefComplexField(createFieldMem(columns))
  def apply(rows: Int, columns: Int) =
    new RefComplexField(createFieldMem(rows, columns))
  def apply(layers: Int, rows: Int, columns: Int) =
    new RefComplexField(createFieldMem(layers, rows, columns))

  // Create a 0-dimensional RefComplexField holding "value" at its only point.
  def apply(value: Complex): RefComplexField = {
    val mem = createFieldMem()
    mem.write(value)
    new RefComplexField(mem)
  }

  // Create RefComplexField of 1, 2, 3 dimensions filled by "f"
  def apply(columns: Int, f: (Int) => Complex): RefComplexField = {
    val mem = createFieldMem(columns)
    for (c <- 0 until columns)
      mem.write(f(c), c)
    new RefComplexField(mem)
  }
  def apply(rows: Int, cols: Int, f: (Int, Int) => Complex): RefComplexField = {
    val mem = createFieldMem(rows, cols)
    for (r <- 0 until rows; c <- 0 until cols)
      mem.write(f(r, c), r, c)
    new RefComplexField(mem)
  }
  def apply(layers: Int, rows: Int, cols: Int, f: (Int, Int, Int) => Complex): RefComplexField = {
    val mem = createFieldMem(layers, rows, cols)
    for (l <- 0 until layers; r <- 0 until rows; c <- 0 until cols)
      mem.write(f(l, r, c), l, r, c)
    new RefComplexField(mem)
  }

  /** Create a random RefComplexField of shape "sizes". */
  def random(sizes: Int*): RefComplexField = {
    val mem = createFieldMem(sizes: _*)
    for (indices <- mem.fieldType.fieldShape.indices)
      mem.write(randomComplex, indices : _*)
    new RefComplexField(mem)
  }

  /** Create a random RefScalarField of shape 'shape'. */
  def random(shape: Shape): RefComplexField =
    random(shape.toArray:_*)

  /** Create a 1D scalar field from a vector. */
  def apply(vector: ComplexVector): RefComplexField = {
    val mem = createFieldMem(vector.length)
    for (i <- 0 until vector.length)
      mem.write(vector(i), i)
    new RefComplexField(mem)
  }

  /** Create a 2D scalar field from a matrix. */
  def apply(matrix: ComplexMatrix): RefComplexField = {
    val mem = createFieldMem(matrix.rows, matrix.columns)
    for (row <- 0 until matrix.rows; col <- 0 until matrix.columns)
      mem.write(matrix(row, col), row, col)
    new RefComplexField(mem)
  }

  /** Create a 2D scalar field from a function that returns a ComplexMatrix. */
  def apply(rows: Int, cols: Int, complexMatrixMaker: () => ComplexMatrix): RefComplexField = {
    val fieldType = createFieldType(rows, cols)
    def complexBufferMaker = {
      val matrix: ComplexMatrix = complexMatrixMaker()
      new RefComplexFieldMemory(fieldType) {
        for (row <- 0 until rows; col <- 0 until cols)
          write(matrix(row,col), row, col)
      }
    }
    new RefComplexField(fieldType, complexBufferMaker) {
      // The GPUEvaluator class currently uses a HashMap to quickly check
      // for operations it has already compiled, but this calls hashCode for
      // this class, which instantiates the potentially large data array on
      // the Host node.  To prevent this, we override the default
      // ComplexRefScalarField.hashCode that is based on the data, choosing instead
      // the simpler, though legitimate, hashCode based only on the fieldType.
      override def hashCode = fieldType.hashCode
    }
  }


  def apply(rows: Int, cols: Int, values: Iterable[Complex]): RefComplexField = {
    val fieldType = createFieldType(rows, cols)
    def complexBufferMaker = {
      RefComplexFieldMemory(fieldType, values)
    }
    new RefComplexField(fieldType, complexBufferMaker) {
      // The GPUEvaluator class currently uses a HashMap to quickly check
      // for operations it has already compiled, but this calls hashCode for
      // this class, which instantiates the potentially large data array on
      // the Host node.  To prevent this, we override the default
      // ComplexRefScalarField.hashCode that is based on the data, choosing instead
      // the simpler, though legitimate, hashCode based only on the fieldType.
      override def hashCode = fieldType.hashCode
    }
  }

  def fromRealParts(rows: Int, cols: Int, values: Iterable[Float]): RefComplexField = {
    val fieldType = createFieldType(rows, cols)
    def complexBufferMaker = {
      RefComplexFieldMemory.fromRealParts(fieldType, values)
    }
    new RefComplexField(fieldType, complexBufferMaker) {
      // The GPUEvaluator class currently uses a HashMap to quickly check
      // for operations it has already compiled, but this calls hashCode for
      // this class, which instantiates the potentially large data array on
      // the Host node.  To prevent this, we override the default
      // ComplexRefScalarField.hashCode that is based on the data, choosing instead
      // the simpler, though legitimate, hashCode based only on the fieldType.
      override def hashCode = fieldType.hashCode
    }
  }

  def randomComplex = Complex(rand.nextFloat, rand.nextFloat)
}
