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

import cogx.platform.types._
import cogx.platform.types.ElementTypes._

/** A fragment representing the field output from a kernel.
  *
  * @param input The fragment driving this output.
  * @param fieldType The type of the output field.
  * @param index Index of the output field.
  * @param owner The abstract kernel that owns and drives this fragment.
  * @param clType The type of value produced by this fragment.
  *
  * @author Greg Snider
  */
private[cogx]
class OutputFieldFragment (val input: Fragment,
                           val fieldType: FieldType,
                           index: Int,
                           val owner: AbstractKernel,
                           clType: CLType)
        extends FieldFragment(clType)
{
  require(owner != null)

  /** Name for the field. */
  val name = OutputFieldFragment.name(index)

  /** Generates an OpenCL output parameter declaration for this field. */
  def outDeclaration = fieldType.elementType match {
    case Uint8Pixel =>
      "    write_only image2d_t " + name
    case Complex32 =>
      "    __global float *" + name
    case _ =>
      "    __global float *" + name
  }

  /** String containing #define statements describing output field geometry. */
  def outParameters: String = FieldDefines(name, fieldType)

  /** Generate #undef statements to clean up #defines. */
  def cleanup: String = FieldDefines.cleanup(name, fieldType)

  val code = ""

  /** The OpenCL code that executes the final assignment of a value to fragment. */
  def writeResult(addressing: AddressingMode) = {
    require(input.isInstanceOf[CodeFragmentOutput],
      "Expecting CodeFragmentOutput as input to this OutputFieldFragment")
    val outFragComment = "\n    // Output fragment " + index + "\n"
    input.asInstanceOf[CodeFragmentOutput].getWriteType match {
      case WriteNull =>
        ""
      case WriteUnknown =>
        ""
      case WriteLocal =>
        outFragComment + writeLocal(addressing, input.read(addressing))
      case WriteNonlocal =>
        outFragComment + writeNonlocal(addressing, input.read(addressing))
      case WriteElementNonlocal =>
        outFragComment + writeElementNonlocal(addressing, input.read(addressing))
    }
  }

  /** An output field has one child, the fragment that drives it. */
  def children = Array(input)

  /** String representation for debugging. */
  override def toString = "FieldFragment " + owner.id
  /** Generate a string to read a tensor of an input field.
    * @param isLocal Does the read use the default row, column, layer values for the thread?
    */
  def _readTensor(isLocal: Boolean) =
    throw new RuntimeException("read of OutputFieldFragment not expected.")

  /** Generate a string to read a tensor element of an input field.
    * @param isLocal Does the read use the default row, column, layer values for the thread?
    */
  def _readTensorElement(isLocal: Boolean) =
    throw new RuntimeException("read of OutputFieldFragment not expected.")
}

private [cogx] object OutputFieldFragment {
  /** Name of the i-th output field */
  def name(index: Int) = "_out_field_" + index
}

