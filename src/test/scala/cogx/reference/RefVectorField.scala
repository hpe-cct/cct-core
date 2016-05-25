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
import cogx.platform.types.ElementTypes.Float32
import cogx.reference.memory.RefFieldMemory
import cogx.cogmath.geometry.Shape
import cogx.cogmath.algebra.real.{Scalar, Vector, Tensor}
import java.util.Random


/** A field holding real vectors.
  *
  * @param data The vectors held in the field.
  *
  * @author Greg Snider
  */
class RefVectorField private[cogx] (val data: RefFieldMemory)
        extends RefField(data.fieldType)
{
  require(data.fieldType.tensorShape.dimensions == 1)

  /** Create a vector field initialized with "data". */
  private[cogx] def this(fieldShape: Shape, vectorShape: Shape, data: Array[Float]) =
    this(new RefFieldMemory(
      new FieldType(fieldShape, vectorShape, Float32), data))

  /** Create a zero-filled vector field. */
  private[cogx] def this(fieldShape: Shape, vectorShape: Shape) =
    this(new RefFieldMemory(
      new FieldType(fieldShape, vectorShape, Float32)))

  // Element access:
  def read(indices: Int*): Vector = data.read(indices : _*).asInstanceOf[Vector]

  def copy = new RefVectorField(data.copy)

  // Subfield access:
  def subfield(columns: Range*) =
    new RefVectorField(data.subfield(columns : _*))

  def downsample(factor:Int, phase:Int) =
    new RefVectorField(data.downsample(factor, phase))

  // Scalar algebra:
  def +(that: Float) = new RefVectorField(data.mapFloat(_ + that))
  def -(that: Float) = new RefVectorField(data.mapFloat(_ - that))
  def *(that: Float) = new RefVectorField(data.mapFloat(_ * that))
  def /(that: Float) = new RefVectorField(data.mapFloat(_ / that))
  def %(that: Float) = new RefVectorField(data.mapFloat(_ % that))
  def unary_-() = map(_ * -1)
  private[cogx] def reciprocal = map(v => v.reciprocal)

  // Vector algebra:
  def +(that: Vector) = map(_ + that)
  def -(that: Vector) = map(_ - that)
  def :*(that: Vector) = map(_ :* that)
  def :/(that: Vector) = map(_ :/ that)

  // Field algebra:
  def +(that: RefVectorField) = new RefVectorField(data.combine(that.data, _ + _))
  def -(that: RefVectorField) = new RefVectorField(data.combine(that.data, _ - _))
  def :*(that: RefVectorField) = new RefVectorField(data.combine(that.data, _ * _))
  def :/(that: RefVectorField) = new RefVectorField(data.combine(that.data, _ / _))
  def %(that: RefVectorField) = new RefVectorField(data.combine(that.data, _ % _))

  def +(that: RefScalarField) = combine(that, (a: Vector, b: Float) => a + b)
  def -(that: RefScalarField) = combine(that, (a: Vector, b: Float) => a - b)
  def :*(that: RefScalarField) = combine(that, (a: Vector, b: Float) => a * b)
  def :/(that: RefScalarField) = combine(that, (a: Vector, b: Float) => a / b)

  // Map, Reduce, Combine:
  def map(f: (Vector) => Vector): RefVectorField = {
    def ff(tensor: Tensor): Tensor = f(tensor.asInstanceOf[Vector])
    // The result vector may not be the same length as the input vector.
    // We deduce the output size by calling f on a dummy vector.
    val trialVector = new Vector(fieldType.tensorShape(0))
    val outVector = f(trialVector)
    val resultFieldType =
      if (outVector.length == trialVector.length)
        fieldType
      else
        new FieldType(fieldType.fieldShape, Shape(outVector.length), Float32)
    new RefVectorField(RefFieldMemory(resultFieldType, data.map(ff)))
  }
  def reduce(f: (Vector, Vector) => Vector): Vector = {
    def ff(a: Tensor, b: Tensor): Tensor =
      f(a.asInstanceOf[Vector], b.asInstanceOf[Vector])
    data.reduceLeft(ff).asInstanceOf[Vector]
  }
  def combine(that: RefVectorField, f: (Vector, Vector) => Vector): RefVectorField = {
    def ff(a: Tensor, b: Tensor): Tensor =
      f(a.asInstanceOf[Vector], b.asInstanceOf[Vector])
    new RefVectorField(RefFieldMemory(fieldType,
      (data zip that.data).map(v => ff(v._1, v._2))))
  }
  def combine(that: RefScalarField, f: (Vector, Float) => Vector): RefVectorField = {
    def ff(a: Tensor, b: Tensor): Tensor =
      f(a.asInstanceOf[Vector], b.read(0))
    new RefVectorField(RefFieldMemory(fieldType,
      (data zip that.data).map(v => ff(v._1, v._2))))
  }

  def dot(that: RefVectorField): RefScalarField = {
    val scalarFieldType =
      new FieldType(fieldType.fieldShape, Shape(), Float32)
    if (that.fieldShape.dimensions == 0) {
      val thatVector = that.read().asInstanceOf[Vector]
      val resultType = new FieldType(fieldShape, Shape(), Float32)
      val resultMem = new RefFieldMemory(resultType)
      for (index: Array[Int] <- fieldShape.indices) {
        val vector = data.read(index : _*).asInstanceOf[Vector]
        val correlation: Float = vector dot thatVector
        resultMem.write(new Scalar(correlation), index : _*)
      }
      new RefScalarField(resultMem)
    } else {
      new RefScalarField(RefFieldMemory.fromFloats(scalarFieldType,
        (data zip that.data).map(v => v._1.asInstanceOf[Vector] dot v._2.asInstanceOf[Vector])))
    }
  }

  /** Correlate "this" and "that" on a point-by-point basis. The result is a
    * scalar field, with each point in that field representing the dot product
    * of the tensor at the corresponding point in "this" and the single
    * tensor "that". This differs from dot(RealFieldBase) in that a single tensor
    * is dotted with each element of "this", instead of a unique tensor for
    * each point.
    */
  def crossDot(thatField: RefScalarField): RefScalarField = {
    val thatVector = thatField.toTensor[Vector]
    val resultType = new FieldType(fieldShape, Shape(), Float32)
    val resultMem = new RefFieldMemory(resultType)
    for (index: Array[Int] <- fieldShape.indices) {
      val vector = data.read(index : _*).asInstanceOf[Vector]
      val correlation: Float = vector dot thatVector
      resultMem.write(new Scalar(correlation), index : _*)
    }
    new RefScalarField(resultMem)
  }

  def reverseCrossDot(that: RefScalarField): RefScalarField = {
    require(this.fieldShape == that.fieldShape)
    val reduced: Vector = (this :* that).reduce(_ + _)
    val columns = this.tensorShape(0)
    RefScalarField(columns, (col) => reduced(col))
  }

  /** Slice out a subfield along a first "index", producing a smaller field of
    * one smaller dimensions.
    */
  def slice(index: Int) =
    new RefVectorField(data.slice(index))

  /** Stack "this" field with 1 or more "other" fields to increase the
    * dimension of the result by 1. All fields must have exactly the same
    * shape to be stackable.
    */
  def stack(other: RefVectorField*) =
    new RefVectorField(data.stack(other.map(_.data) : _*))

  /** Extract the component indexed by "tensorIndex" in each vector in the
    * field, returning a scalar field with the extracted components.
    */
  def sliceTensor(tensorIndex: Int) =
    new RefScalarField(data.sliceTensor(tensorIndex))

  private[cogx] def asPixelArray = data.asPixelArray
  private[cogx] def asRawArray = data.asRawArray

  /** Compare "this" and "that" for approximate equality. */
  def ~==(that: RefVectorField): Boolean = {
    data ~== that.data
  }

  // Value equality:
  override def equals(that: Any): Boolean = {
    if (that.isInstanceOf[RefVectorField])
      data == that.asInstanceOf[RefVectorField].data
    else
      false
  }
  override def hashCode: Int = data.hashCode()

  /** Print out a vector field for debugging. */
  def print {
    synchronized {
      for (element <- 0 until tensorShape(0)) {
        println("RefVectorField.vectorElement(" + element + ")")
        sliceTensor(element).print
      }
    }
  }
}

/** Factory for creating real scalar fields.
  *
  * @author Greg Snider
  */
object RefVectorField {
  private lazy val rand = new Random

  // Create N-dimensional vector fields.
  def apply(vectorShape: Shape) =
    new RefVectorField(Shape(), vectorShape)
  def apply(columns: Int, vectorShape: Shape) =
    new RefVectorField(Shape(columns), vectorShape)
  def apply(rows: Int, columns: Int, vectorShape: Shape) =
    new RefVectorField(Shape(rows, columns), vectorShape)
  def apply(layers: Int, rows: Int, columns: Int, vectorShape: Shape) =
    new RefVectorField(Shape(layers, rows, columns), vectorShape)
  def apply(fieldShape: Shape, vectorShape: Shape) =
    new RefVectorField(fieldShape, vectorShape)

  /** Create a 0 RefVectorField containing "vector". */
  def apply(vector: Vector): RefVectorField = {
    new RefVectorField(Shape(), vector.shape) {
      data.write(vector)
    }
  }

  /** Create an N-dimensional vector field populated with vector "init" */
  def apply(fieldShape: Shape, init: Vector): RefVectorField = {
    (new RefVectorField(fieldShape, init.shape)).map(_ => init)
  }

  /** Create a 1D RefVectorField of shape "columns" filled by "f" */
  def apply(columns: Int, f: Int => Vector): RefVectorField = {
    val vectorLength = f(0).length
    new RefVectorField(Shape(columns), Shape(vectorLength)) {
      for (c <- 0 until columns)
        this.data.write(f(c), c)
    }
  }

  /** Create a 2D RefVectorField of shape "rows" x "columns" filled by "f". */
  def apply(rows: Int, columns: Int, f: (Int, Int) => Vector): RefVectorField = {
    val vectorLength = f(0, 0).length
    new RefVectorField(Shape(rows, columns), Shape(vectorLength)) {
      for (r <- 0 until rows; c <- 0 until columns)
        this.data.write(f(r, c), r, c)
    }
  }

  // This form of apply() is not present in MatrixField, because I feel
  // it should be removed- it appears to be unnecessary given the simpler
  // apply above.  -RJC
  /** Create a 2D RefVectorField of shape "rows" x "columns" with the tensor of size vectorLength, filled by "f". */
  def apply(rows: Int, columns: Int, vectorLength: Int, f: (Int, Int) => Vector): RefVectorField = {
    new RefVectorField(Shape(rows, columns), Shape(vectorLength)) {
      for (r <- 0 until rows; c <- 0 until columns)
        this.data.write(f(r, c), r, c)
    }
  }

  /** Create a 3D RefVectorField of shape "rows" x "columns" x "layers". */
  def apply(layers: Int, rows: Int, columns: Int, f: (Int, Int, Int) => Vector):
     RefVectorField =
  {
    val vectorLength = f(0, 0, 0).length
    new RefVectorField(Shape(layers, rows, columns), Shape(vectorLength)) {
      for (l <- 0 until layers; r <- 0 until rows; c <- 0 until columns)
        this.data.write(f(l, r, c), l, r, c)
    }
  }

  private def random(fieldType: FieldType): RefVectorField =
    new RefVectorField(RefFieldMemory.random(fieldType))

  /** Create a random RefVectorField. */
  /** Modified by M.Pickett 3/8/2013*/
  def random(fieldShape:Shape, vectorShape: Shape): RefVectorField = {
    val fieldType = new FieldType(fieldShape, vectorShape, Float32)
    random(fieldType)
  }

  /** Create a random 0D RefVectorField. */
  /** Modified by M.Pickett 8/1/2012*/
  def random(vectorShape: Shape): RefVectorField = {
    val fieldType = new FieldType(Shape(), vectorShape, Float32)
    random(fieldType)
  }

  /** Create a random 1D RefVectorField of shape "columns". */
  def random(columns: Int, vectorShape: Shape): RefVectorField = {
    val fieldType = new FieldType(Shape(columns), vectorShape, Float32)
    random(fieldType)
  }

  /** Create a random 2D RefVectorField of shape "rows" x "columns". */
  def random(rows: Int, columns: Int, vectorShape: Shape): RefVectorField = {
    val fieldType = new FieldType(Shape(rows, columns), vectorShape, Float32)
    random(fieldType)
  }

  /** Create a random 3D RefVectorField of shape "layers" x "rows" x "columns". */
  def random(layers: Int, rows: Int, columns: Int, vectorShape: Shape): RefVectorField = {
    val fieldType = new FieldType(Shape(layers, rows, columns), vectorShape, Float32)
    random(fieldType)
  }
}
