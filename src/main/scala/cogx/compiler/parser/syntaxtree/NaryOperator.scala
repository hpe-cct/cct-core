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

package cogx.compiler.parser.syntaxtree

import cogx.compiler.parser.semantics.SemanticError
import cogx.platform.types.FieldType
import cogx.platform.types.ElementTypes.Uint8Pixel
import cogx.compiler.parser.op._
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.cogmath.geometry.Shape
import scala.Some

/** Factory for creating binary operators. This checks types to make sure
  * the operands may be legally combined, and computes the result type using
  * the Cog rules.
  *
  * @author Greg Snider
  */
private[cogx]
object NaryOperator extends SemanticError {

  private def requireFieldDim(inType: FieldType, dim: Int) {
    if (inType.dimensions != dim)
      fieldDimensionError(inType.dimensions, dim)
  }

  private def requireTensorDim(inType: FieldType, tensorDim: Int) {
    if (inType.tensorOrder != tensorDim)
      tensorDimensionError(inType.tensorOrder, tensorDim)
  }

  private def require2DScalarField(inType: FieldType) {
    requireFieldDim(inType, 2)
    requireTensorDim(inType, 0)
  }
  /** Create a built-in binary operator operating on two fields, performing
    * full semantic checking.
    *
    * @param operation The operation performed on the two fields.
    * @param in Array of input fields.
    *
    * @author Greg Snider
    */
  def apply[T <: Field](operation: NaryOpcode, in: Array[T]): Field = {
    val inType = in.map(_.fieldType).toArray

    /** Semantic check: is the operation permitted for this size/type of input? */
    operation match {
      case MergeColorPlanesOp =>
        for(i <- 1 until in.length)
          if (inType(i) != inType(0))
            error("Mismatched input field types: " + inType(i) + " " + inType(0))
        for(i <- 0 until in.length)
          require2DScalarField(inType(i))
      case FieldArraySelectOp =>
        if (in.length < 2)
          error("Too few arguments to select")
        requireFieldDim(inType(0), 0)
        requireTensorDim(inType(0), 0)
        for(i <- 2 until in.length)
          if (inType(i) != inType(1))
            error("Mismatched input field types: " + inType(i) + " " + inType(1))
      case TensorStackOp(newTensorShape) =>
        // All fields should be of the same fieldType
        if (inType.map(_ != inType(0)).foldLeft(false)(_ || _))
          typeError(operation, inType)
      case StackOp =>
        // All fields should be of the same fieldType
        if (inType.map(_ != inType(0)).foldLeft(false)(_ || _))
          typeError(operation, inType)
      case DomainFilterColumnsOp =>
        if (!isColorField(inType(0)))
          error("May only be applied to color fields")
        if (!isTensor0Field(inType(1)))
          error("The domain transform field must be a scalar field")
        if (inType(0).fieldShape(0) != inType(1).fieldShape(1))
          error("The two fields must have transposed shapes. Found: " +
                  inType(0).fieldShape + " and " + inType(1).fieldShape)
        if (inType(0).fieldShape(1) != inType(1).fieldShape(0))
          error("The two fields must have transposed shapes. Found: " +
                  inType(0).fieldShape + " and " + inType(1).fieldShape)
        if (inType(2).fieldShape.dimensions != 0)
          error("Illegal field for carrying box filter radius")
        if (inType(2).tensorShape.dimensions != 0)
          error("Illegal field for carrying box filter radius")
      case DomainFilterRowsOp =>
        val imageType = inType(0)
        val guideType = inType(1)
        val radiusType = inType(2)
        if (isColorField(imageType)) {
          if (!isRealField(guideType))
            error("The domain transform field must be a scalar field")
          if (imageType.fieldShape != guideType.fieldShape)
            error("Both fields must have the same shape. Found: " +
                    imageType.fieldShape + " and " + guideType.fieldShape)
        } else if (isRealField(imageType)) {
          if (imageType.fieldShape != guideType.fieldShape)
            error("Both fields must have the same shape. Found: " +
                    imageType.fieldShape + " and " + guideType.fieldShape)
          if (guideType.tensorShape.dimensions != 0)
            if (imageType.tensorShape != guideType.tensorShape)
              error("Both fields must have the same tensor shape. Found: " +
                      imageType.tensorShape + " and " + guideType.tensorShape)
        } else
          error("illegal field type for this operator")
        if (radiusType.fieldShape.dimensions != 0)
          error("Illegal field for carrying box filter radius")
        if (radiusType.tensorShape.dimensions != 0)
          error("Illegal field for carrying box filter radius")
      case default => // OK by default
    }

    /** The FieldType of the result of the operation */
    val outType: FieldType = operation match {
      // If you add an operation that produces a different type than the
      // default type, add that here. E.g. "case MyOp => new NodeType(...)"
      case MergeColorPlanesOp =>
        new FieldType(inType(0).fieldShape, Shape(inType.length), Uint8Pixel)
      case FieldArraySelectOp =>
        // inType(0) is the selection index, inType(1) is the first of the
        // fields being selected.
        inType(1)
        // Building up a 2x3 MatrixField from 6 ScalarFields will appear the
        // same as building up a 3x2 MatrixField at this point, except for the
        // newTensorShape (which is why we need that parameter of the Opcode).
      case TensorStackOp(newTensorShape) =>
        inType(0).resizeTensor(newTensorShape)
      case StackOp =>
        in(0).fieldType.incrementDimensions(in.length)
      case DomainFilterColumnsOp =>
        in(0).fieldType
      case DomainFilterRowsOp =>
        in(0).fieldType
      case default =>
        if (inType.map(_ != inType(0)).foldLeft(false)(_ || _))
          typeError(operation, inType)
        inType(0)
    }

    /** Produce the result Field */
    Field(operation, in.asInstanceOf[Array[Field]], outType)
  }
}
