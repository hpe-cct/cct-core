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

import cogx.cogmath.algebra.real.{Scalar, Tensor, Matrix}
import cogx.platform.types.FieldType
import cogx.platform.types.ElementTypes.Float32
import cogx.reference.memory.RefFieldMemory
import cogx.cogmath.geometry.Shape
import java.util.Random

/** A field holding real matrices.
  *
  * @param data The linearized data held in the field, stored so that last
  *        index varies most quickly, first index most slowly.
  *
  * @author Greg Snider
  */
class RefMatrixField private[cogx] (val data: RefFieldMemory)
        extends RefField(data.fieldType)
{
  require(data.fieldType.tensorShape.dimensions == 2)

  /** Create a matrix field initialized with "data". */
  private[cogx] def this(fieldShape: Shape, matrixShape: Shape, data: Array[Float]) =
    this(new RefFieldMemory(
      new FieldType(fieldShape, matrixShape, Float32), data))

  /** Create a zero-filled matrix field. */
  private[cogx] def this(fieldShape: Shape, matrixShape: Shape) =
    this(new RefFieldMemory(
      new FieldType(fieldShape, matrixShape, Float32)))

  // Element access:
  def read(indices: Int*): Matrix = {
    data.read(indices : _*).asInstanceOf[Matrix]
  }

  /** Print out a matrix field for debugging. */
  def print {
    println("RefMatrixField:")
    for (indices: Array[Int] <- fieldType.fieldShape.indices) {
      println("(" + indices.map(_.toString).reduceLeft(_ + " " + _) + ")")
      read(indices: _*).print
    }
  }

  def copy = new RefMatrixField(data.copy)

  // Subfield access:
  def subfield(columns: Range*) =
    new RefMatrixField(data.subfield(columns : _*))

  def downsample(factor:Int, phase:Int) =
    new RefMatrixField(data.downsample(factor, phase))

  // Scalar algebra:
  def +(that: Float) = map(_ + that)
  def -(that: Float) = map(_ - that)
  def *(that: Float) = map(_ * that)
  def /(that: Float) = map(_ / that)
  def unary_-() = map(_ * -1)
  private[cogx] def reciprocal = map(m => m.reciprocal)

  // Field algebra:
  def +(that: RefMatrixField) = combine(that, (a: Matrix, b: Matrix) => a + b)
  def -(that: RefMatrixField) = combine(that, (a: Matrix, b: Matrix) => a - b)
  def :*(that: RefMatrixField) = combine(that, (a: Matrix, b: Matrix) => a :* b)
  def :/(that: RefMatrixField) = combine(that, (a: Matrix, b: Matrix) => a :/ b)

  def +(that: RefScalarField) = combine(that, (a: Matrix, b: Float) => a + b)
  def -(that: RefScalarField) = combine(that, (a: Matrix, b: Float) => a - b)
  def :*(that: RefScalarField) = combine(that, (a: Matrix, b: Float) => a * b)
  def :/(that: RefScalarField) = combine(that, (a: Matrix, b: Float) => a / b)

  // Map, Reduce, Combine:
  def map(f: (Matrix) => Matrix): RefMatrixField = {
    def ff(tensor: Tensor): Tensor = f(tensor.asInstanceOf[Matrix])
    // The result matrix may not have the same shape as the input matrix.
    // We deduce the output size by calling f on a dummy matrix.
    val trialMatrix = new Matrix(fieldType.tensorShape(0),fieldType.tensorShape(1))
    val outMatrix = f(trialMatrix)
    val resultFieldType =
      if (outMatrix.shape == outMatrix.shape)
        fieldType
      else
        new FieldType(fieldType.fieldShape, Shape(outMatrix.length), Float32)
    new RefMatrixField(RefFieldMemory(resultFieldType, data.map(ff)))
  }
  def reduce(f: (Matrix, Matrix) => Matrix): Matrix = {
    def ff(a: Tensor, b: Tensor): Tensor =
      f(a.asInstanceOf[Matrix],b.asInstanceOf[Matrix])
    data.reduceLeft(ff).asInstanceOf[Matrix]
  }
  def combine(that: RefMatrixField, f: (Matrix, Matrix) => Matrix): RefMatrixField = {
    def ff(a: Tensor, b: Tensor): Tensor =
      f(a.asInstanceOf[Matrix],b.asInstanceOf[Matrix])
    new RefMatrixField(RefFieldMemory(fieldType,
      (data zip that.data).map(v => ff(v._1, v._2))))
  }
  def combine(that: RefScalarField, f: (Matrix, Float) => Matrix): RefMatrixField = {
    def ff(a: Tensor, b: Tensor): Tensor =
      f(a.asInstanceOf[Matrix], b.read(0))
    new RefMatrixField(RefFieldMemory(fieldType,
      (data zip that.data).map(v => ff(v._1, v._2))))
  }

  // Point-wise dot product
  def dot(that: RefMatrixField): RefScalarField = {
    val scalarFieldType =
      new FieldType(fieldType.fieldShape, Shape(), Float32)
    new RefScalarField(RefFieldMemory.fromFloats(scalarFieldType,
      (data zip that.data).map(v => v._1.asInstanceOf[Matrix] dot v._2.asInstanceOf[Matrix])))
  }

  /** Correlate "this" and "that" on a point-by-point basis. The result is a
    * scalar field, with each point in that field representing the dot product
    * of the tensor at the corresponding point in "this" and the single
    * tensor "that". This differs from dot(RealFieldBase) in that a single tensor
    * is dotted with each element of "this", instead of a unique tensor for
    * each point.
    */
  def crossDot(thatField: RefScalarField): RefScalarField = {
    val thatMatrix = thatField.toTensor[Matrix]
    val resultType = new FieldType(fieldShape, Shape(), Float32)
    val resultMem = new RefFieldMemory(resultType)
    for (index: Array[Int] <- fieldShape.indices) {
      val matrix = data.read(index : _*).asInstanceOf[Matrix]
      val correlation: Float = matrix dot thatMatrix
      resultMem.write(new Scalar(correlation), index : _*)
    }
    new RefScalarField(resultMem)
  }

  def reverseCrossDot(that: RefScalarField): RefScalarField = {
    require(this.fieldShape == that.fieldShape)
    val reduced: Matrix = (this :* that).reduce(_ + _)
    val rows = this.tensorShape(0)
    val columns = this.tensorShape(1)
    RefScalarField(rows, columns, (row, col) => reduced(row, col))
  }

  /** Slice out a subfield along a first "index", producing a smaller field of
    * one smaller dimensions.
    */
  def slice(index: Int) =
    new RefMatrixField(data.slice(index))

  /** Stack "this" field with 1 or more "other" fields to increase the
    * dimension of the result by 1. All fields must have exactly the same
    * shape to be stackable.
    */
  def stack(other: RefMatrixField*) =
    new RefMatrixField(data.stack(other.map(_.data) : _*))

  /** Extract the component indexed by "tensorIndex" in each vector in the
    * field, returning a scalar field with the extracted components.
    */
  def sliceTensor(tensorRowIndex: Int, tensorColIndex: Int) =
    new RefScalarField(data.sliceTensor(tensorRowIndex, tensorColIndex))

  private[cogx] def asPixelArray = data.asPixelArray
  private[cogx] def asRawArray = data.asRawArray

  /** Compare "this" and "that" for approximate equality. */
  def ~==(that: RefMatrixField): Boolean = {
    data ~== that.data
  }

  // Value equality:
  override def equals(that: Any): Boolean = {
    if (that.isInstanceOf[RefMatrixField])
      data == that.asInstanceOf[RefMatrixField].data
    else
      false
  }
  override def hashCode: Int = data.hashCode
}

/** Factory for creating real matrix fields.
  *
  * @author Greg Snider
  */
object RefMatrixField {
  private lazy val rand = new Random

  // Create N-dimensional matrix fields.
  def apply(matrixShape: Shape) =
    new RefMatrixField(Shape(), matrixShape)
  def apply(columns: Int, matrixShape: Shape) =
    new RefMatrixField(Shape(columns), matrixShape)
  def apply(rows: Int, columns: Int, matrixShape: Shape) =
    new RefMatrixField(Shape(rows, columns), matrixShape)
  def apply(layers: Int, rows: Int, columns: Int, matrixShape: Shape) =
    new RefMatrixField(Shape(layers, rows, columns), matrixShape)
  def apply(fieldShape: Shape, matrixShape: Shape) =
    new RefMatrixField(fieldShape, matrixShape)

  /** Create a 0 RefMatrixField containing "matrix". */
  def apply(matrix: Matrix): RefMatrixField = {
    new RefMatrixField(Shape(), matrix.shape) {
      data.write(matrix)
    }
  }

  /** Create a 1D RefMatrixField of shape "columns" filled by "f" */
  def apply(columns: Int, f: Int => Matrix): RefMatrixField = {
    val matrixShape = Shape(f(0).rows, f(0).columns)
    new RefMatrixField(Shape(columns), matrixShape) {
      for (c <- 0 until columns)
        this.data.write(f(c), c)
    }
  }

  /** Create a 2D RefMatrixField of shape "rows" x "columns" filled by "f". */
  def apply(rows: Int, columns: Int, f: (Int, Int) => Matrix): RefMatrixField = {
    val matrixShape = Shape(f(0, 0).rows, f(0, 0).columns)
    new RefMatrixField(Shape(rows, columns), matrixShape) {
      for (r <- 0 until rows; c <- 0 until columns)
        this.data.write(f(r, c), r, c)
    }
  }

  /** Create a 3D RefMatrixField of shape "rows" x "columns" x "layers". */
  def apply(layers: Int, rows: Int, columns: Int, f: (Int, Int, Int) => Matrix):
  RefMatrixField =
  {
    val matrixShape = Shape(f(0, 0, 0).rows, f(0, 0, 0).columns)
    new RefMatrixField(Shape(layers, rows, columns), matrixShape) {
      for (l <- 0 until layers; r <- 0 until rows; c <- 0 until columns)
        this.data.write(f(l, r, c), l, r, c)
    }
  }


  /** Create an N-D matrix field with shape "fieldShape" initialized by "f". */
  def apply(fieldShape: Shape, matrixShape: Shape)(f: (Array[Int]) => Matrix) =
    new RefMatrixField(fieldShape, matrixShape) {
      for (indices <- fieldShape.indices)
        this.data.write(f(indices), indices : _*)
    }

  private def random(fieldType: FieldType): RefMatrixField =
    new RefMatrixField(RefFieldMemory.random(fieldType))


  /** Create a random 0D RefMatrixField of shape "rows". */
  /** Modified by M. Pickett 8/1/2012*/
  def random(matrixShape: Shape): RefMatrixField  = {
    val fieldType = new FieldType(Shape(), matrixShape, Float32)
    random(fieldType)
  }

  /** Create a random 1D RefMatrixField of shape "rows". */
  def random(cols: Int, matrixShape: Shape): RefMatrixField  = {
    val fieldType = new FieldType(Shape(cols), matrixShape, Float32)
    random(fieldType)
  }

  /** Create a random 2D RefMatrixField of shape "rows" x "columns". */
  def random(rows: Int, columns: Int, matrixShape: Shape): RefMatrixField  = {
    val fieldType = new FieldType(Shape(rows, columns), matrixShape, Float32)
    random(fieldType)
  }

  /** Create a random 3D RefMatrixField of shape "rows" x "columns" x "layers". */
  def random(layers: Int, rows: Int, columns: Int, matrixShape: Shape): RefMatrixField  = {
    val fieldType = new FieldType(Shape(layers, rows, columns), matrixShape, Float32)
    random(fieldType)
  }

  /** Create a fieldShape RefMatrixField populated with identity matrices of size matrixShape. */
  def identity(fieldShape:Shape, matrixShape:Int):RefMatrixField = {
    new RefMatrixField(fieldShape,Shape(matrixShape,matrixShape)){
      for (indices <- fieldShape.indices)
        this.data.write(Matrix.identity(matrixShape), indices : _*)
    }
  }

  private def randomMatrix(shape: Shape): Matrix = {
    val rows = shape(0)
    val columns = shape(1)
    val matrix = new Matrix(rows, columns).randomize
    matrix
  }
}
