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

package cogx.compiler.codegenerator.common

import cogx.platform.types._
import cogx.platform.types.ElementTypes._
import cogx.cogmath.geometry.Shape

/**
 * Object that provides the properties of a field that might be considered as
 * 'policy' judgements.
 *
 * @author Greg Snider and Dick Carter
 */
private[cogx]
object FieldPolicies {
  private val MaxTensorPoints = 4

  val EnableLength8SmallTensors = false
  val EnableLength16SmallTensors = false

  /** Return true if `fieldType` is a small tensor field, i.e. one whose
    * tensors can fit in a single variable, e.g. float4. */
  def isSmallTensorField(fieldType: FieldType): Boolean = {
    if (isComplexField(fieldType))
      isTensor0Field(fieldType)
    else {
      fieldType.tensorShape.points <= MaxTensorPoints ||
      fieldType.tensorShape.points == 8 && EnableLength8SmallTensors ||
      fieldType.tensorShape.points == 16 && EnableLength16SmallTensors
    }
  }

  /** Return true if `fieldType` is a complex tensor field. */
  def isComplexField(fieldType: FieldType): Boolean = {
    fieldType.elementType == Complex32
  }

  /** Return true if `fieldType` is a real tensor field. */
  def isRealField(fieldType: FieldType): Boolean = {
    fieldType.elementType == Float32
  }

  /** Return true if `fieldType` is a color field. */
  def isColorField(fieldType: FieldType): Boolean = {
    fieldType.elementType == Uint8Pixel
  }

  /** Return true if `fieldType` has tensors of order 0 (i.e. simple scalars) */
  def isTensor0Field(fieldType: FieldType) = fieldType.tensorOrder == 0

  /** Return true if `fieldType` has tensors of order 0 (i.e. simple scalars) */
  def isTensor1Field(fieldType: FieldType) = fieldType.tensorOrder == 1

  /** Return true if `fieldType` has a 0-dimension fieldShape */
  def is0DField(fieldType: FieldType) = fieldType.dimensions == 0

  /** Return true if `fieldType` is a `realField` and a `tensor0Field`. */
  def isTensor0RealField(fieldType: FieldType) =
    isTensor0Field(fieldType) && isRealField(fieldType)

  /** Return true if `fieldType` is a big tensor field. */
  def isBigTensorField(fieldType: FieldType) = !isSmallTensorField(fieldType)

  /** maps 'fieldType' to a 0 dimensional field */
  def to0D(fieldType: FieldType): FieldType = fieldType.resize(Shape())

  /** maps 'fieldType' to a real field */
  def toReal(fieldType: FieldType): FieldType = {
    new FieldType(fieldType.fieldShape, fieldType.tensorShape, Float32)
  }

  /** maps 'fieldType' to a complex field */
  def toComplex(fieldType: FieldType): FieldType = {
    new FieldType(fieldType.fieldShape, fieldType.tensorShape, Complex32)
  }

  /** maps 'fieldType' to a color field */
  def toColor(fieldType: FieldType): FieldType = {
    new FieldType(fieldType.fieldShape, fieldType.tensorShape, Uint8Pixel)
  }
}
