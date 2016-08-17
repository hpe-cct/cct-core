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

import cogx.cogmath.geometry.Shape
import cogx.platform.checkpoint.{Saveable, RestoreFactory, ObjectSaver, ObjectRestorer}
import cogx.platform.types.ElementTypes._


/** The type of a node in the syntax tree.
  *
  * @param fieldShape Shape of the field produced by the node.
  * @param tensorShape Shape of then tensors in the field produced by the node.
  * @param elementType Type of element (Real, Float, ...) in the tensors.
    *
  * @author Greg Snider
  */
@SerialVersionUID(-7776775079664549648L)
class FieldType(val fieldShape: Shape,
                val tensorShape: Shape,
                val elementType: ElementType) extends Serializable with Saveable
{
  /** Number of dimensions in the field. */
  val dimensions = fieldShape.dimensions

  /** Order of the tensors i the field. */
  val tensorOrder = tensorShape.dimensions

  /** Returns a field with `newShape` field shape, with the same tensorShape and
    * elementType as `this`.
    */
  def resize(newShape: Shape): FieldType =
    new FieldType(newShape, tensorShape, elementType)

  /** Returns a field with the same field shape and elementType as `this`, but
    * with the tensorShape `newShape`..
    */
  def resizeTensor(newShape: Shape): FieldType =
    new FieldType(fieldShape, newShape, elementType)

  /** Reduces the fieldShape in each dimension. */
  def downsample(factor: Int): FieldType = resize(fieldShape.downsample(factor))

  /** Increases the fieldShape by a factor in each dimension. */
  def upsample(factor: Int): FieldType =
    resize(new Shape(fieldShape.toArray.map(_ * factor)))

  /** Drop the first "d" dimensions, returning a smaller-shaped field. */
  def drop(d: Int): FieldType = resize(fieldShape.drop(d))

  /** Drop the last "d" dimensions, returning a smaller-shaped field. */
  def dropLast(d: Int): FieldType = resize(fieldShape.dropLast(d))

  /**
   * Increase the number of dimensions of the field by 1, with
   * "newDimensionSize" being equal to the size of the new dimension.
   */
  def incrementDimensions(newDimensionSize: Int): FieldType =
    resize(Shape(newDimensionSize) concatenate fieldShape)

  // The following code was copied from FieldMemoryLayout, but should probably
  // exist in only one place.  XXX   -RJC

  /** Logical size of the field, expressed colloquially. */
  val (layers, rows, columns) = {
    dimensions match {
      case 0 =>
        (1, 1, 1)
      case 1 =>
        (1, 1, fieldShape(0))
      case 2 =>
        (1, fieldShape(0), fieldShape(1))
      case 3 =>
        (fieldShape(0), fieldShape(1), fieldShape(2))
      case x =>
        require(requirement = false, "inconsistent limit on dimensions")
        (0, 0, 0)
    }
  }

  // The following suggests that a vector has 1 row and N columns.  Yet
  // elsewhere within the Cog software, the transpose of a vector yields
  // a matrix with 1 row and N columns.   -RJC  XXX

  /** Logical size of the field's tensors, expressed colloquially. */
  val (tensorRows, tensorColumns) = {
    tensorShape.dimensions match {
      case 0 =>
        (1, 1)
      case 1 =>
        (1, tensorShape(0))
      case 2 =>
        (tensorShape(0), tensorShape(1))
      case x =>
        require(requirement = false, "inconsistent limit on tensor dimensions")
        (0, 0)
    }
  }

  /** Is this FieldType an image (requiring an OpenCL Image Memory, not a Buffer Memory)? */
  def isImage = elementType == Uint8Pixel

  /** Compare `this` and `that` for value equality. */
  override def equals(that: Any): Boolean = {
    that match {
      case f: FieldType =>
        (fieldShape == f.fieldShape) &&
                (tensorShape == f.tensorShape) &&
                (elementType == f.elementType)
      case _ =>
        false
    }
  }

  /** Required because of override of equals. */
  override val hashCode: Int = fieldShape.hashCode + tensorShape.hashCode

  /** Print a string representing the node type for debugging. */
  override def toString = {
    elementType match {
      case Float32 =>
        tensorOrder match {
          case 0 =>
            "ScalarField" + fieldShape.toString("")
          case 1 =>
            "VectorField" + fieldShape.toString("") + tensorShape.toString("")
          case 2 =>
            "MatrixField" + fieldShape.toString("") + tensorShape.toString("")
          case 3 =>
            "Tensor3Field" + fieldShape.toString("") + tensorShape.toString("")
          case x =>
            require(requirement = false, "unknown tensor order: " + x)
            ""
        }
      case Complex32 =>
        tensorOrder match {
          case 0 =>
            "ComplexField" + fieldShape.toString("")
          // Currently unsupported, but print something reasonable anyway...
          case 1 =>
            "ComplexVectorField" + fieldShape.toString("") + tensorShape.toString("")
          case 2 =>
            "ComplexMatrixField" + fieldShape.toString("") + tensorShape.toString("")
          case 3 =>
            "ComplexTensor3Field" + fieldShape.toString("") + tensorShape.toString("")
          case x =>
            require(requirement = false, "unknown tensor order: " + x)
            ""
        }
      case Uint8Pixel =>
        "ColorField" + fieldShape.toString("") + tensorShape.toString("")
      case x =>
        require(requirement = false, "todo: " + x)
        ""
    }
  }

  /** Save this FieldType instance using the facilities of the ObjectSaver */
  def save(saver: ObjectSaver) {
    saver.writeIntArray("fieldShape", fieldShape.toArray)
    saver.writeIntArray("tensorShape", tensorShape.toArray)
    saver.writeString("elementType", elementType.name)
  }
 }

/** Factory object for creating FieldTypes from their stored representations. */
object FieldType extends RestoreFactory {
  /** Create a FieldType instance through use of the provided ObjectRestorer
    * @param restorer The restorer through which to read the new object state.
    * @return The created FieldType based on the read information.
    */
  def restore(restorer: ObjectRestorer): FieldType = {
    val fieldShapeArray = restorer.readIntArray("fieldShape")
    val tensorShapeArray = restorer.readIntArray("tensorShape")
    val elementTypeString = restorer.readString("elementType")
    new FieldType(Shape(fieldShapeArray), Shape(tensorShapeArray), ElementTypes(elementTypeString))
  }
}
