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

/** Generates an OpenCL code fragment. A code fragment generates a single result
  * in a single output variable which may be of any CL type supported by Cog.
  * An example of the pattern:
  * {{{
  *    float temp5;
  *    {
  *        temp5 = (do some computation here);
  *    }
  * }}}
  *
  * @param inputs The input fragments driving input values to this fragment.
  * @param addressing Addressing mode for this fragment.
  * @param implementation User-defined code string for this fragment.
  * @param clType The type of the value produced by this fragment
  * @param fieldType The field type of the result produced by this fragment
  *
  * @author Greg Snider
  */

private[cogx]
class CodeFragmentOutput(input: CodeFragment,
                   index: Int)
        extends Fragment
{
  /** Name for this fragment, used for output variable. */
  val name = input.name(index)

  /** The name of the CodeFragment */
  def read(addressing: AddressingMode) = input.read(addressing, index)

  /** The indexing mode of the write operation (@out0 = , @outNonlocal0 =, etc.) */
  def setOutputIndex(globalIndex: Int) { input.setOutputIndex(globalIndex, index) }

  def clType = input.clTypes(index)

  def getWriteType = input.getWriteType(index)

  /** Synthesizes OpenCL code using `implementation`. Replaces read references
    * to inputs such as `read(@in0)` and `readNonlocal(@in1)` to
    * appropriate OpenCL code for reading the input. This also replaces any
    * `writeTensor(value)` statements it encounters.
    */
  def code = ""

  val inputs = Array(input.asInstanceOf[Fragment])

  /** Inputs of this CodeFragment that are CodeFragments.
    *
    * @return Sequence of code fragments
    */
  def children = inputs

  override def toString =
    "CodeFragmentIndexer " + name + "(" + inputs.map(_.toString).reduceLeft(_ + " " + _) + ")"

  /** Generate a string to read a tensor of an input field.
    * @param isLocal Does the read use the default row, column, layer values for the thread?
    */
  def _readTensor(isLocal: Boolean) = input._readTensor(isLocal, index)

  /** Generate a string to read a tensor element of an input field.
    * @param isLocal Does the read use the default row, column, layer values for the thread?
    */
  def _readTensorElement(isLocal: Boolean) = input._readTensorElement(isLocal, index)
}
