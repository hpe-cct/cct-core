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

import cogx.cogmath.geometry.Shape
import cogx.platform.types.ElementTypes.ElementType
import cogx.platform.types.FieldType


/** Base class for a discrete, immutable tensor field. Each point in the field
  * contains a tensor of arbitrary shape, but all tensors in the field
  * have identical shape.
  *
  * @param fieldShape Shape of the field.
  * @param tensorShape Shape of the tensor at each point in the field.
  *
  * @author Greg Snider
  */
abstract class RefField(val fieldShape: Shape,
                     val tensorShape: Shape,
                     val element: ElementType)
{
  def this(fieldType: FieldType) =
    this (fieldType.fieldShape, fieldType.tensorShape, fieldType.elementType)

  /** Get the type information for the field. */
  val fieldType = new FieldType(fieldShape, tensorShape, element)

  /** Number of dimensions for this field. */
  val dimensions = fieldShape.dimensions

  /** Synonym for fieldShape(0). */
  val columns = fieldType.columns

  /** Synonym for fieldShape(1). */
  val rows = fieldType.rows

  /** Synonym for fieldShape(2). */
  val layers = fieldType.layers

  /** Number of locations or points in the field. */
  val points = fieldShape.points

  /** The size of the tensor at each point in the field (number of values). */
  val tensorSize = tensorShape.points

  /** Number of numbers in the field. */
  val numbers = points * tensorSize

  /** The order (or rank) of the tensor at each point in the field. */
  val tensorOrder = tensorShape.dimensions

  /** Create a string descriptor of the field. */
  override def toString: String = {
    "Field " + fieldShape.toString + " " + tensorShape.toString
  }

  /** INTERNAL USE ONLY--to speed up transfers to GPU. */
  private[cogx] def asRawArray: Array[Float]

  /** Make an exact copy of this Field. */
  //protected[cog] def copy: Field
}
