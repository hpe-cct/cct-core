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

import cogx.platform.types.{VirtualFieldRegister, FieldType}
import cogx.platform.types.ElementTypes.Uint8Pixel

/** An input field on a kernel. This is a "slot" that initially is unconnected
  * to a fragment on its input.
  *
  * @param fieldType The type of the input field.
  * @param index Index of the input field.
  * @param inputRegisters Input virtual field registers for the kernel that owns this fragment.
  * @param clType The type of value produced by this fragment.
  *
  * @author Greg Snider
  */
private[cogx]
class InputFieldFragment(val fieldType: FieldType, index: Int,
                         inputRegisters: Array[VirtualFieldRegister],
                         clType: CLType)
        extends FieldFragment(clType)
{
  var constant = false

  /** An input field fragment may be bound to a driving input that effectively
    * short-circuits the input.
    */
  private var drivingInput: Fragment = null

  /** Name for the field.  There's some subtlety here: during the merging
    * process, inputs can accumulate a chain of old InputFieldFragments.
    * If this InputFieldFragment is driven by such a chain of InputFieldFragments,
    * all but the last are effectively ignored when calculating name().  If the
    * last InputFieldFragment has a null input, then the _in_field_ name form is
    * reported, else if the last is driven by a CodeFragment, then the
    * CodeFragment's name is reported.
    */
  def name = {
    if (drivingInput == null) {
      "_in_field_" + index
    } else
      drivingInput.name
  }

  /** Generate a string to read the "current" element of an input field. */
  override def read(addressing: AddressingMode) =
    if (drivingInput == null) {
        super.read(addressing)
    }  else {
      drivingInput.read(addressing)
    }

  /** Generate a string to read element (row, column) of an input field. */
  override def readNonlocal(addressing: AddressingMode): String = {
    if (drivingInput == null)
      super.readNonlocal(addressing)
    else {
      drivingInput match {
        case in: FieldFragment =>
          in.readNonlocal(addressing)
        case x =>
          throw new RuntimeException("Indexed read on non-field fragment: " +
             x.getClass)
      }
    }
  }

  /** Generates an OpenCL input parameter declaration for this field. */
  def inDeclaration = {
    require(drivingInput == null)
    if (fieldType.elementType == Uint8Pixel) {
      fieldType.dimensions match {
        case 2 => "    read_only image2d_t " + name
        case 3 => "    read_only image3d_t " + name
        case x => throw new RuntimeException("Unsupported ColorField field dimension: " + x)
      }
    } else {
      val constType = if (constant) "    __constant " else "    __global const "
      constType + "float *" + name
    }
  }

  /** Generates a string of #define statements describing field geometry. */
  def inParameters: String = {
    require(drivingInput == null)
    FieldDefines(name, fieldType)
  }

  /** Generate #undef statements to clean up #defines. */
  def cleanup: String = FieldDefines.cleanup(name, fieldType)

  /** Bind "input" fragment as driving this. */
  def bindDrivingInput(input: Fragment) {
    require(input != null)
    require(drivingInput == null)
    drivingInput = input
    // Copy 'constant' tag on this input to any InputFieldFragment that wraps it
    drivingInput match {
      case in: InputFieldFragment => in.constant = constant
      case x =>
    }
  }

  /** Inputs to this fragment, none if no driving input. */
  def children =
    if (drivingInput == null) Array[Fragment]()
    else Array(drivingInput)

  /** Code for this fragment. */
  val code = ""

  /** ID for this fragment. */
  def id = inputRegisters(index).source.id

  def registerDriver = inputRegisters(index)

  override def toString = "InputFieldFragment(" + id + ")"

  /** Generate a string to read a tensor of an input field.  The driving input will be
    * non-null for merged kernels.  The chaining call may stop with a CodeFragment if the input
    * is driven by the output of an embedded merged kernel, or it may terminate with a null
    * if the input is a primary field input of the merged kernel.
    *
    * @param isLocal Does the read use the default row, column, layer values for the thread?
    */
  def _readTensor(isLocal: Boolean) =
    if (drivingInput == null) {
      super.readTensor(isLocal)
    }  else {
      drivingInput._readTensor(isLocal)
    }

  /** Generate a string to read a tensor element of an input field.  The driving input will be
    * non-null for merged kernels.  The chaining call may stop with a CodeFragment if the input
    * is driven by the output of an embedded merged kernel, or it may terminate with a null
    * if the input is a primary field input of the merged kernel.
    *
    * @param isLocal Does the read use the default row, column, layer values for the thread?
    */
  def _readTensorElement(isLocal: Boolean) =
    if (drivingInput == null) {
      super.readTensorElement(isLocal)
    }  else {
      drivingInput._readTensorElement(isLocal)
    }

}