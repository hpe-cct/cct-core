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

package cogx.compiler.codegenerator.opencl.fragments

import cogx.platform.types.FieldType
import cogx.platform.types.ElementTypes._


/** Base class for fragments representing fields.
  *
  * @param clType The OpenCL type for an element in the field. This is CLFloat
  *        for scalarFields, CLFloat2, CLFloat3, ... for tensor fields.
  *
  * @author Greg Snider
  */
private[cogx]
abstract class FieldFragment(val clType: CLType) extends Fragment {

  /** The type of field represented by this fragment. */
  val fieldType: FieldType

  /////////////////////////////////////////////////////////////////////////////
  //
  // The following methods are used by GPUOperators exclusively.
  //
  /////////////////////////////////////////////////////////////////////////////

  /** Generate a string for reading a tensor in a tensor field.
    *
    * @param isLocal True for local (_layer, _row, _column) addressing, false
    *                for explicit addressing.
    * @return String for the read.
    */
  def readTensor(isLocal: Boolean): String = {
    FieldIO.readTensor(fieldType, name, clType, fieldLocal = isLocal)
  }

  /** Generate a string for reading a tensor element in a tensor field.
    *
    * @param isLocal True for local (_layer, _row, _column) addressing, false
    *                for explicit addressing.
    * @return String for the read.
    */
  def readTensorElement(isLocal: Boolean): String = {
    FieldIO.readElement(fieldType, name, fieldLocal = isLocal, tensorLocal = false)
  }

  /** Generate a string for writing a tensor in a tensor field.
    *
    * @param isLocal True for local (_layer, _row, _column) addressing, false
    *                for explicit addressing.
    * @return String for the write.
    */
  def writeTensor(value: String, isLocal: Boolean): String = {
    FieldIO.writeTensor(fieldType, name, clType, fieldLocal = isLocal, value)
  }

  /** Generate a string for writing a tensor element in a tensor field.
    *
    * @param isLocal True for local (_layer, _row, _column) addressing, false
    *                for explicit addressing.
    * @return String for the write.
    */
  def writeTensorElement(value: String, isLocal: Boolean): String = {
    FieldIO.writeElement(fieldType, name, fieldLocal = isLocal, tensorLocal = false, value)
  }


  /////////////////////////////////////////////////////////////////////////////
  //
  // The remaining methods are deprecated, suitable only for internal
  // HyperKernels. Once we transition to GPUOperators, these will go away.
  //
  /////////////////////////////////////////////////////////////////////////////

  /** Write out the result to the "current" tensor element in the output field.
    *
    * @param addressing Addressing mode.
    * @param result Output value to write.
    * @return String to perform the write.
    */
  protected def writeLocal(addressing: AddressingMode, result: String): String = {
    addressing match {
      case SmallTensorAddressing =>
        FieldIO.writeTensor(fieldType, name, clType, fieldLocal = true, result)
      case TensorElementAddressing =>
        FieldIO.writeElement(fieldType, name, fieldLocal = true,
          tensorLocal = true, result)
      case BigTensorAddressing =>
        ""
//        throw new RuntimeException("illegal call")
    }
  }

  /** Write out the result to the "current" tensor element in the output field
    *  within a non-local field point specified by row, column and layer.
    *
    * @param addressing Addressing mode.
    * @param result Output value to write.
    * @return String to perform the write.
    */
  protected def writeNonlocal(addressing: AddressingMode, result: String): String = {
    addressing match {
      case SmallTensorAddressing =>
        FieldIO.writeTensor(fieldType, name, clType, fieldLocal = false, result)
      case TensorElementAddressing =>
        FieldIO.writeElement(fieldType, name, fieldLocal = false,
          tensorLocal = false, result)
      case BigTensorAddressing =>
        throw new RuntimeException("illegal call")
    }
  }

  /** Write out the result to the "current" tensor element in the output field
    *  within a non-local field point specifed by row, column and layer.
    *
    * @param addressing Addressing mode.
    * @param result Output value to write.
    * @return String to perform the write.
    */
  protected def writeElementNonlocal(addressing: AddressingMode, result: String): String = {
    addressing match {
      case SmallTensorAddressing =>
        throw new RuntimeException("illegal call")
      case TensorElementAddressing =>
        FieldIO.writeElement(fieldType, name, fieldLocal = false,
          tensorLocal = false, result)
      case BigTensorAddressing =>
        throw new RuntimeException("illegal call")
    }
  }


  /** Read an element in a field indexed by (layer, row, column) or
    * as appropriate for the dimensionality of the field. For example, a 2D
    * scalar field named "foo" will generate the reference:
    * {{{
    *     "foo[row * foo_row_stride + column]"
    * }}}
    * while for a 2D color field it will generate the reference:
    * {{{
    *     "read_imagef(foo, sampler, (int2)(column, row)
    * }}}
    * Note that the user must use the names "layer", "row" and "column" for
    * the indices, there is no other option.
    *
    * Also note that "read_imagef" uses (x, y) addressing instead of
    * (row, column).
    *
    * @param addressing Addressing mode.
    * @return String representing the value read from the field.
    */
  def readNonlocal(addressing: AddressingMode): String = {
    addressing match {
      case BigTensorAddressing =>
        //throw new RuntimeException("Illegal 'read' for addressing mode")
        // Synonym
        readElementNonlocal(addressing)
      case SmallTensorAddressing =>
        FieldIO.readTensor(fieldType, name, clType, fieldLocal = false)
      case TensorElementAddressing =>
        FieldIO.readElement(fieldType, name, fieldLocal = false, tensorLocal = false)
    }
  }

  /** Generate a string to read the "local" element in a field, located at
    * location (_layer, _row, _column).
    *
    * @param addressing Addressing mode.
    * @return String representing value read from a field.
    */
  def read(addressing: AddressingMode): String = {
    addressing match {
      case BigTensorAddressing =>
        //throw new RuntimeException("Illegal 'read' for addressing mode")
        // Synonym
        readElement(addressing)
      case SmallTensorAddressing =>
        FieldIO.readTensor(fieldType, name, clType, fieldLocal = true)
      case TensorElementAddressing =>
        FieldIO.readElement(fieldType, name, fieldLocal = true, tensorLocal = true)
    }
  }

  /** Generate a string to read the "local" element in a field, located at
    * location (_layer, _row, _column) (tensorElement). Note that
    * tensorElement must be explicitly provided.
    *
    * @param addressing Addressing mode.
    * @return String representing value read from a field.
    */
  def readElement(addressing: AddressingMode): String = {
    addressing match {
      case BigTensorAddressing =>
        FieldIO.readElement(fieldType, name, fieldLocal = true, tensorLocal = false)
      case SmallTensorAddressing =>
        FieldIO.readElement(fieldType, name, fieldLocal = true, tensorLocal = false)
      case TensorElementAddressing =>
        // Synonym
        read(addressing)
        //throw new RuntimeException("Illegal 'readElement' for addressing mode")
    }
  }

  /** Generate a string to read the "local" element in a field, located at
    * location (layer, row, column) (tensorElement). Note that
    * tensorElement must be explicitly provided.
    *
    * @param addressing Addressing mode.
    * @return String representing value read from a field.
    */
  def readElementNonlocal(addressing: AddressingMode): String = {
    addressing match {
      case BigTensorAddressing =>
        FieldIO.readElement(fieldType, name, fieldLocal = false, tensorLocal = false)
      case SmallTensorAddressing =>
        FieldIO.readElement(fieldType, name, fieldLocal = false, tensorLocal = false)
      case TensorElementAddressing =>
        // Synonym
        readNonlocal(addressing)
        //throw new RuntimeException("Illegal 'readElement' for addressing mode")
    }
  }

  /** Read the single point in a 0D non-scalar field. This only works for
    * non-image fields for now.
    *
    * @param addressing Addressing mode.
    * @return String representing a read of the single point.
    */
  def readPoint(addressing: AddressingMode): String = {
    require(fieldType.elementType == Float32 ||
            fieldType.elementType == Complex32)
    read(addressing)
  }

  /** Read the single point in a 0D scalar field.
    *
    * There's a bit of a kludge here: we must look at 'name' to see
    * if it's of the _tempNNN_ form, in which case we won't index it by [0]
    *
    * @return String representing a read of the single point.
    */
  def readScalar(addressing: AddressingMode): String = {
    require(fieldType.elementType == Float32 ||
            fieldType.elementType == Complex32)
    if (UniqueID.findFirstIn(name) != null)
      "(" + name + ")"
    else if (fieldType.elementType == Float32)
      "(" + name + "[0])"
    else
      "((float2) (" + name + "[0]," + name + "[" + name + "_partStride]))"
  }
}