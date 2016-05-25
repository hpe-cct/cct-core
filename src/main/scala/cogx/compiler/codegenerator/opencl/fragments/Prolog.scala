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

import cogx.platform.opencl.WorkGroupParameters
import cogx.platform.types.FieldType

/** Creates the prolog code in a kernel that extracts the field address
  * parameters from the OpenCL runtime system. This creates the following
  * Cog global variables (the "_" prefix denotes that it is a Cog variable):
  * {{{
  *   _layer
  *   _row
  *   _column
  *   _tensorElement
  * }}}
  * If local addressing is used, it also generates the following Cog variables:
  * {{{
  *   _localLayer
  *   _localRow
  *   _localColumn
  * }}
  * Two other constants, _outputPointIndex  and _outputIndex, are no longer
  * generated since read indexes are always done relative to the input field
  * and the output index generation has been expanded to include indexing.
  *
  * @param workFieldType Type of field used to set work parameters.
  * @param addressing Addressing mode for this kernel.
  * @param local True if local addressing (e.g. for tiling an input field into
  *        local memory) is used.
  * @param samplingMode True if positions outside the domain of the field should
  *        return the value of the nearest border, otherwise return 0. This is
  *        valid only for image fields.
  * @param needsSampler True if the kernel will be doing a read_image call,
  *        which requires a sampler variable.
  *
  * Author: Greg Snider
  */
private[cogx]
class Prolog(workFieldType: FieldType,
             workGroup: WorkGroupParameters,
             addressing: AddressingMode,
             local: Boolean,
             samplingMode: SamplingMode,
             needsSampler: Boolean)
{
  private val tensorSize = workFieldType.tensorShape.points


  //USE #defines here whenever possible, then add cleanup method and call in code synthesis

  /** Return OpenCL code for the prolog. */
  def code: String = {
    val buffer = new StringBuffer("    // Prolog\n")
    buffer.append("    // addressing mode = " + addressing.toString + "\n")
    buffer.append(s"    // global workgroup (layers, rows, columns) = " +
      s"(${workGroup.globalLayers}, ${workGroup.globalRows}, ${workGroup.globalColumns})\n")
    if (needsSampler)
      buffer.append("    const sampler_t sampler = " +
              samplingMode.flag + " | CLK_FILTER_NEAREST;\n")

    // Some parts of this package assume that a 0D field has 1 "column",
    // but we'd like to get away from that fiction.  Don't even define
    // _column for 0D fields to flush out code snippets that should
    // probably not be using _column to begin with.

    // We always define _layer, _row, and _column to support GPUOperators code generation-
    // If a user GPUOperator has a workfield that's a 2D vectorfield and does a "semi-local" read
    // of a 3D input field, as in _readTensorElement(field3D, elementIndex), what should happen?  It
    // turns out that the GPUOperator code generator at the point of translation does not have access to
    // the workField dimension, so it can't throw an exception to outlaw this case.  Instead it optimistically
    // assumes that a read of a 3D field must come from a 3D workgroup and it produces code like:

    // column = _column;
    // row = _row;
    // layer = _layer;
    // tensorElement = elementIndex;
    // <readNonLocal based on layer, row, column and tensorElement>
    //
    // So you see that even though the workfield is 2D (and would not normally have _layer defined),
    // we need to have the _layer variable defined (as 0) for the above sequence to work.

    if (workFieldType.dimensions >= 1)
      buffer.append("    const int _column = get_global_id(0);\n")
    else
      buffer.append("    const int _column = 0;\n")

    if (workFieldType.dimensions >= 2)
      buffer.append("    const int _row = get_global_id(1);\n")
    else
      buffer.append("    const int _row = 0;\n")

    addressing match {
      case SmallTensorAddressing =>
        if (workFieldType.dimensions == 3)
          buffer.append("    const int _layer = get_global_id(2);\n")
        else
          buffer.append("    const int _layer = 0;\n")
        buffer.append("    const int _tensorElement = 0;\n")
      case TensorElementAddressing =>
        workFieldType.dimensions match {
          case 3 =>
            // In the third dimension, the workgroups are laid out as:
            // layer 0, tensor element 0        get_global_id(2) = 0
            // layer 1, tensor element 0        get_global_id(2) = 1
            // layer 2, tensor element 0        get_global_id(2) = 2
            // layer 0, tensor element 1        get_global_id(2) = 3
            // layer 1, tensor element 1        get_global_id(2) = 4
            // layer 2, tensor element 1        get_global_id(2) = 5
            // This has not been performance tested against another approach
            // where the tensor element index cycles fastest (see commented out
            // alternative below).
            // These formulas may need to be recast if the localLayers != 1  XXX
            buffer.append("    const int _layer = get_global_id(2) % " + workFieldType.layers + ";\n")
            buffer.append("    const int _tensorElement = get_global_id(2) / " + workFieldType.layers + ";\n")
//            buffer.append("    const int _layer = get_global_id(2) / " + tensorSize + ";\n")
//            buffer.append("    const int _tensorElement = get_global_id(2) % " + tensorSize + ";\n")
          case 2 =>
            buffer.append("    const int _layer = 0;\n")
            buffer.append("    const int _tensorElement = get_global_id(2);\n")
          case 1 =>
            buffer.append("    const int _layer = 0;\n")
            buffer.append("    const int _tensorElement = get_global_id(1);\n")
          case 0 =>
            buffer.append("    const int _layer = 0;\n")
            buffer.append("    const int _tensorElement = get_global_id(0);\n")
        }
      case BigTensorAddressing =>
        if (workFieldType.dimensions == 3)
          buffer.append("    const int _layer = get_global_id(2);\n")
        else
          buffer.append("    const int _layer = 0;\n")
    }

    // Useful defines for non-local field writes, defined at
    // the outermost scope so that a kernel doing such a write can pass
    // out the needed indexing information from its kernel scope {}.
    // Best to define all possible variables, rather than figure out which
    // may be needed.

    buffer.append("    int layer = 0, row = 0, column = 0, tensorElement = 0;\n")

    workFieldType.dimensions match {
      case 3 =>
        buffer.append("    const int _localLayer = get_local_id(2);\n")
        buffer.append("    const int _localRow = get_local_id(1);\n")
        buffer.append("    const int _localColumn = get_local_id(0);\n")

        buffer.append("    const int _groupLayer = get_group_id(2);\n")
        buffer.append("    const int _groupRow = get_group_id(1);\n")
        buffer.append("    const int _groupColumn = get_group_id(0);\n")

      case 2 =>
        buffer.append("    const int _localRow = get_local_id(1);\n")
        buffer.append("    const int _localColumn = get_local_id(0);\n")

        buffer.append("    const int _groupRow = get_group_id(1);\n")
        buffer.append("    const int _groupColumn = get_group_id(0);\n")

      case 1 =>
        buffer.append("    const int _localColumn = get_local_id(0);\n")
        buffer.append("    const int _groupColumn = get_group_id(0);\n")

      case 0 =>
        buffer.append("    const int _localColumn = get_local_id(0);\n")
        buffer.append("    const int _groupColumn = get_group_id(0);\n")
    }

    buffer.toString
  }

  /** Escape clause if we fall outside bounds of field.  With the default
    * fieldName of "", the routine checks the thread against the field used
    * to generate the work group parameters (whose defines are _rows, _columns,
    * etc.).
    */
  def returnIfOutOfBounds(fieldName: String = "", fieldType: FieldType = workFieldType): String = {
    val conditionString = addressing match {
      case SmallTensorAddressing =>
        fieldType.dimensions match {
          case 3 => "    if (_layer >= %fn%_layers || _row >= %fn%_rows || _column >= %fn%_columns)\n"
          case 2 => "    if (_row >= %fn%_rows || _column >= %fn%_columns)\n"
          case 1 => "    if (_column >= %fn%_columns)\n"
          case 0 => "    if (get_global_id(0) >= 1)\n"    // Only 1st thread is active
        }
      case TensorElementAddressing =>
        fieldType.dimensions match {
          case 3 => "    if (_layer >= %fn%_layers || _row >= %fn%_rows || " +
                  "_column >= %fn%_columns || _tensorElement >= %fn%_tensorElements)\n"
          case 2 => "    if (_row >= %fn%_rows || _column >= %fn%_columns || " +
                  "_tensorElement >= %fn%_tensorElements)\n"
          case 1 => "    if (_column >= %fn%_columns || " + "" +
                  "_tensorElement >= %fn%_tensorElements)\n"
          case 0 => "    if (_tensorElement >= %fn%_tensorElements)\n"
        }
      case BigTensorAddressing =>
        fieldType.dimensions match {
          case 3 => "    if (_layer >= %fn%_layers || _row >= %fn%_rows || _column >= %fn%_columns)\n"
          case 2 => "    if (_row >= %fn%_rows || _column >= %fn%_columns)\n"
          case 1 => "    if (_column >= %fn%_columns)\n"
          case 0 => "    if (get_global_id(0) >= 1)\n"    // Only 1st thread is active
        }
    }
    conditionString.replace("%fn%", fieldName) + "        return;\n"
  }

  // If the Hyperkernel subclass has set up the work group parameters
  // based on a field other than the output field, then the kernel code may
  // want to insert the following code as well, before output generation, to
  // prune threads that are outside of the output field dimensions.
  def returnIfOutOfOutputFieldBounds(index: Int, fieldType: FieldType) =
    returnIfOutOfBounds(OutputFieldFragment.name(index), fieldType)
}
