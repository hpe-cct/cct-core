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
import cogx.compiler.codegenerator.common.FieldPolicies._

/** Generates an OpenCL code fragment. A code fragment generates one or more results
  * in output variables which may be of any CL type supported by Cog.
  * An example of the pattern for a single output:
  * {{{
  *    float temp5;
  *    {
  *        temp5 = (do some computation here);
  *    }
  * }}}
  *
  * An example of the pattern for multiple outputs:
  * {{{
  *    float temp5_0;
  *    float temp5_1;
  *    {
  *        temp5_0 = (do some computation here);
  *        temp5_1 = (do some computation here);
  *    }
  * }}}
  *
  * @param inputs The input fragments driving input values to this fragment.
  * @param addressing Addressing mode for this fragment.
  * @param implementation User-defined code string for this fragment.
  * @param clTypes The types of the values produced by this fragment
  * @param fieldTypes The field types of the results produced by this fragment
  * @author Greg Snider
  */

private[cogx]
class CodeFragment(val inputs: Array[Fragment],
                   addressing: AddressingMode,
                   implementation: String,
                   val clTypes: Array[CLType],
                   fieldTypes: Array[FieldType])
        extends Fragment
{
  /** Declaration of fragment output variables. */
  private def declaration(index: Int) = "    " + clTypes(index) + " " + name(index) + ";\n"

  /** Base name for this fragment, used for output variables. */
  val name: String = UniqueID()

  /** Output variable name for the indexed output. */
  def name(index: Int): String = {
    require(index < fieldTypes.length, "Compiler error: improper kernel output indexing.")
    if (fieldTypes.length == 1 && index == 0)
      name
    else
      name + "_" + index
  }

  /** Returns the name of the CodeFragment output */
  def read(addressing: AddressingMode, index: Int) = name(index)

  /** Returns the name of the single CodeFragment output.  This should not be called frankly, since
    * CodeFragments are followed by CodeFragmentOutputs which only call the above indexed read method. */
  def read(addressing: AddressingMode) = {
    require(fieldTypes.length == 1, "Compiler error: improper kernel output indexing.")
    read(addressing, 0)
  }

  /** Generate a string to read a tensor of an input field.
    *
    * @param isLocal Does the read use the default row, column, layer values for the thread?
    */
  def _readTensor(isLocal: Boolean) = {
    require(fieldTypes.length == 1, "Compiler error: improper kernel output indexing.")
    _readTensor(isLocal, 0)
  }

  /** Generate a string to read a tensor of an input field.
    *
    * @param isLocal Does the read use the default row, column, layer values for the thread?
    * @param index The output index for the output being read
    */
  def _readTensor(isLocal: Boolean, index: Int) = {
    require(isLocal, "Non-local read of embedded merged kernel not expected.")
    name(index)
  }

  /** Generate a string to read a tensor element of an input field.
    *
    * @param isLocal Does the read use the default row, column, layer values for the thread?
    */
  def _readTensorElement(isLocal: Boolean) = {
    require(fieldTypes.length == 1, "Compiler error: improper kernel output indexing.")
    _readTensorElement(isLocal, 0)
  }

  /** Generate a string to read a tensor element of an input field.
    *
    * @param isLocal Does the read use the default row, column, layer values for the thread?
    * @param index The output index for the output being read
    */
  def _readTensorElement(isLocal: Boolean, index: Int) = {
    require(isLocal, "Non-local read of embedded merged kernel not expected.")
    val tmp = name(index)
    clTypes(index) match {
      case CLFloat => name(index)
      case CLFloat2 => s"(tensorElement == 0 ? $tmp.s0 : $tmp.s1)"
      case CLFloat3 => s"(tensorElement == 0 ? $tmp.s0 : (tensorElement == 1 ? $tmp.s1 : $tmp.s2))"
      case CLFloat4 => s"(tensorElement <= 1 ? (tensorElement == 0 ? $tmp.s0 : $tmp.s1) : (tensorElement == 2 ? $tmp.s2 : $tmp.s3))"
      case x => throw new RuntimeException(s"_readTensorElement not supported for field of type $x")
    }
  }

  /** The indexing mode of the write operation (@out0 = , @outNonlocal0 =, etc.) for the various outputs. */
  private val writeTypes = createWriteTypes(implementation)

  /** The indexing mode of the write operation (@out0 = , @outNonlocal0 =, etc.) for the given output. */
  def getWriteType(index: Int) = {
    writeTypes(index)
  }

  /** When a BigTensorAddressing mode kernel includes a statement like @outElementNonlocal2, which writes
    * to the third "local" output, this must be translated to something like:
    *
    * _out_field_9 = ...
    *
    * (assuming the third local output was connected to the 10th global output of a merged kernel).  This
    * setOutputIndex method sets this association between the local and global output indices.
    */
  def setOutputIndex(globalIndex: Int, localIndex: Int = 0) { outputIndices(localIndex) = globalIndex }

  /** OutputIndex for this code fragment, needed in BigTensorAddressing mode when
    * the output assignment is performed within the code fragment.
    */
  private val outputIndices = Array.fill(fieldTypes.length)(-1)

  /** The type of the CodeFragment- an obsolete notion given multi-output primitive HyperKernels */
  def clType = {
    throw new RuntimeException("Compiler error: single CLType requested of CodeFragment.")
  }

  /** Synthesizes OpenCL code using `implementation`. Replaces read references
    * to inputs such as `read(@in0)` and `readNonlocal(@in1)` to
    * appropriate OpenCL code for reading the input. This also replaces any
    * `writeTensor(value)` statements it encounters.
    */
  def code = {
    val buffer = new StringBuffer("\n    // Code fragment\n")
    for(i <- 0 until fieldTypes.length)
      buffer.append(declaration(i))
    buffer.append("    {\n")
    buffer.append("    " + implementation + "\n")
    buffer.append("    }\n")

    // Substitute user-named inputs with system names. */
    var result = buffer.toString
    result = translateReads(result)
    result = translateWrites(result)
    result
  }

  private def translateReads(code: String): String = {
    var result = code

    // Note that we have to check for instances of the string we're
    // trying to replace before we call String.replace() to avoid evaluation
    // of the second arg to replace(), which may throw false exceptions, given
    // the wrong field input.  Note the deferred evaluation of the 2nd argument.
    // Returns a Boolean indicating whether any replacements were made.
    def lazyReplace(target: String, replacement: => String): Boolean = {
      if (result.contains(target)) {
        result = result.replace(target, replacement)
        true                                          // replacements made
      }
      else
        false                                         // no replacements made
    }

    var varInputs = 0
    for (index <- 0 until inputs.length) {
      val input = inputs(index)
      def inXArgList = "(@in" + varInputs + ")"
      input match {
        case field: FieldFragment =>
          // Check for read requests, making sure they're legal, and
          // substituting in executable OpenCL code for them. Note that we
          // must check for the names in a fixed order to avoid, for example,
          // confusing "read" with "readNonlocal".
          lazyReplace("readNonlocal" + inXArgList, field.readNonlocal(addressing))
          lazyReplace("readElementNonlocal" + inXArgList, field.readElementNonlocal(addressing))
          lazyReplace("readElement" + inXArgList, field.readElement(addressing))
          lazyReplace("readPoint" + inXArgList, field.readPoint(addressing))
          lazyReplace("readScalar" + inXArgList, field.readScalar(addressing))
          lazyReplace("read" + inXArgList, field.read(addressing))

          // Read requests made by GPUOperators. These replace the above read
          // operations once we transition exclusively to GPUOperators.
          lazyReplace("_readTensorLocal" + inXArgList,
            field._readTensor(isLocal = true))
          lazyReplace("_readTensorNonlocal" + inXArgList,
            field._readTensor(isLocal = false))
          lazyReplace("_readTensorElementLocal" + inXArgList,
            field._readTensorElement(isLocal = true))
          lazyReplace("_readTensorElementNonlocal" + inXArgList,
            field._readTensorElement(isLocal = false))

          // In support of some tricky kernels like the FFT
          lazyReplace("fieldName" + inXArgList, field.name)

          varInputs += 1
        case code: CodeFragment =>
        case x =>
          throw new RuntimeException("Not supported yet: " + x)
      }
    }
    result
  }

  /** Should a non-local write of the FieldType `fieldType` in AddressingMode `addressing` be performed in-place,
    * as opposed to at the final "output section" of the kernel.  Since non-local writes are not forward mergeable,
    * this is always an option.  However, the simple textual substitution that is performed cannot handle
    * SmallTensor fields that are not ScalarFields.  Put another way, the in-place write only works if the CLType is
    * a float, int, etc., and not a more complicated entity like a float2.  Handling an in-place write for a float2
    * of a 1D field by a simple substitution of the @outNonlocal0 keyword would require something like the following
    * non-legal OpenCL syntax:
    *
    * (_out_field_0[_column + 0 * _tensorStride], _out_field_0[_column + 1 * _tensorStride]) = ...
    *
    * @param fieldType  Field type of the field being written.
    * @param addressing  Addressing mode for the write.
    * @return
    */
  private def doInplaceNonlocalWrite(fieldType: FieldType, addressing: AddressingMode): Boolean = {
    addressing match {
      case SmallTensorAddressing =>
        // This covers ScalarFields and VectorFields/MatrixFields of length 1
        fieldType.tensorShape.points == 1
      case TensorElementAddressing => true
      case BigTensorAddressing => true
    }
  }

  /** Perform an analysis of the code for the types of write operations (local, non-local, in-fragment).
    * We can't make the substitutions here though because merging hasn't happened yet and this may change
    * the _out_field_NNN number that will appear in an in-code-fragment write. */
  private def createWriteTypes(code: String) = {
    var result = code
    val writeTypes = Array.fill[WriteType](fieldTypes.length)(WriteUnknown)
    // First get rid of uses of @outN that are not output assignments
    result = result.replace("fieldName(@out", "XXX")
    result = result.replace("partStride(@out", "XXX")
    for(i <- fieldTypes.length-1 to 0 by -1) {
      // Then characterize what type of output assignment there is, for each output
      var writeType = writeTypes(i)
        if (result.contains("@outElementNonlocal" + i) || result.contains("_writeTensorElementNonlocal" + i)) {
        require(writeType == WriteUnknown || writeType == WriteNull,
          "Multiple kernel output statements must be of same type: " + writeType)
          writeType = WriteNull
      }
      if (result.contains("@outElement" + i) || result.contains("_writeTensorElementLocal" + i)) {
        require(writeType == WriteUnknown || writeType == WriteNull,
          "Multiple kernel output statements must be of same type: " + writeType)
        writeType = WriteLocal
      }
      if (result.contains("@outNonlocal" + i) || result.contains("_writeTensorNonlocal" + i)) {
        require(writeType == WriteUnknown || writeType == WriteNull,
          "Multiple kernel output statements must be of same type: " + writeType)
        require(addressing != BigTensorAddressing, "Illegal write")
        writeType = if (doInplaceNonlocalWrite(fieldTypes(i), addressing)) WriteNull else WriteNonlocal
      }
      if (result.contains("@out" + i) || result.contains("_writeTensorLocal" + i)) {
        require(writeType == WriteUnknown,
          "Multiple kernel output statements must be of same type: " + writeType)
        require(addressing != BigTensorAddressing, "Illegal write")
        writeType = WriteLocal
      }
      writeTypes(i) = writeType
    }
    writeTypes
  }

  /** Perform the textual substitutions for the write operations and other output field keywords. */
  private def translateWrites(code: String): String = {
    var result = code

    // Note that we have to check for instances of the string we're
    // trying to replace before we call String.replace() to avoid evaluation
    // of the second arg to replace(), which may throw false exceptions, given
    // the wrong field input.  Note the deferred evaluation of the 2nd argument.
    // Returns whether any replacements were made.

    // The @outFieldName and @outFieldPartStride substitutions were made
    // to prevent some tricky kernels like the FFT from hardcoding text
    // such as _out_field_0.  Hardcoding the output index '0' is dangerous since
    // it would break if the merger ever bundled the FFT kernel with other kernels.

    def lazyReplace(target: String, replacement: => String): Boolean = {
      if (result.contains(target)) {
        result = result.replace(target, replacement)
        true                                          // replacements made
      }
      else
        false                                         // no replacements made
    }
    for(i <- fieldTypes.length-1 to 0 by -1) {
      val outputIndex = outputIndices(i)

      /////////////////////////////////////////////////////////////////////////
      // Write tensor element nonlocal
      /////////////////////////////////////////////////////////////////////////

      // Old-style call
      if (lazyReplace("@outElementNonlocal" + i,
        FieldIO.writePointer(fieldTypes(i), OutputFieldFragment.name(outputIndex),
          fieldLocal = false, tensorLocal = false)))
      {
        require(outputIndex >= 0, "Code gen failure: outputIndex not set.")
        // Accessing the field name allows one to bump the memory pointer, which makes the output
        // no longer forward mergeable.  Thus, we allow such access only when non-local writes are already being
        // performed.  The FFT and DCT kernels use this.
        result = result.replace("fieldName(@out" + i + ")",
          OutputFieldFragment.name(outputIndex))
        result = result.replace("partStride(@out" + i + ")",
          OutputFieldFragment.name(outputIndex) + "_partStride")
      }

      // New-style call (GPU operators)
      if (lazyReplace("_writeTensorElementNonlocal" + i,
        FieldIO.writePointer(fieldTypes(i), OutputFieldFragment.name(outputIndex),
          fieldLocal = false, tensorLocal = false)))
      {
        require(outputIndex >= 0, "Code gen failure: outputIndex not set.")
        // Accessing the field name allows one to bump the memory pointer, which makes the output
        // no longer forward mergeable.  Thus, we allow such access only when non-local writes are already being
        // performed.  The FFT and DCT kernels use this.
        result = result.replace("fieldName(@out" + i + ")",
          OutputFieldFragment.name(outputIndex))
        result = result.replace("partStride(@out" + i + ")",
          OutputFieldFragment.name(outputIndex) + "_partStride")
      }

      /////////////////////////////////////////////////////////////////////////
      // Write tensor nonlocal
      /////////////////////////////////////////////////////////////////////////

      // Old-style call
      if (doInplaceNonlocalWrite(fieldTypes(i), addressing)) {
        lazy val writeText =
          addressing match {
            case SmallTensorAddressing =>
              FieldIO.writeTensor0FieldPointer(fieldTypes(i), OutputFieldFragment.name(outputIndex),
                fieldLocal = false)
            case TensorElementAddressing =>
              FieldIO.writePointer(fieldTypes(i), OutputFieldFragment.name(outputIndex),
                fieldLocal = false, tensorLocal = true)
            case BigTensorAddressing =>
              throw new RuntimeException("illegal write")
          }
        if (lazyReplace("@outNonlocal" + i, writeText)) {
          require(outputIndex >= 0, "Code gen failure: outputIndex not set.")
        }
      }
      else {
        if (lazyReplace("@outNonlocal" + i, name(i))) {
          require(addressing != BigTensorAddressing, "Illegal write")
        }
      }

      // New-style call (GPU operators)
      if (doInplaceNonlocalWrite(fieldTypes(i), addressing)) {
        lazy val writeText =
          FieldIO.writeTensor0FieldPointer(fieldTypes(i), OutputFieldFragment.name(outputIndex),
            fieldLocal = false)
        if (lazyReplace("_writeTensorNonlocal" + i, writeText)) {
          require(outputIndex >= 0, "Code gen failure: outputIndex not set.")
        }
      }
      else {
        if (lazyReplace("_writeTensorNonlocal" + i, name(i))) {
          require(addressing != BigTensorAddressing, "Illegal write")
        }
      }


      /////////////////////////////////////////////////////////////////////////
      // Write tensor element local
      /////////////////////////////////////////////////////////////////////////

      // Old-style call
      if (lazyReplace("@outElement" + i,
        FieldIO.writePointer(fieldTypes(i), OutputFieldFragment.name(outputIndex),
          fieldLocal = true, tensorLocal = false)))
      {
        require(outputIndex >= 0, "Code gen failure: outputIndex not set.")
        result = result.replace("fieldName(@out" + i + ")",
          OutputFieldFragment.name(outputIndex))
        result = result.replace("partStride(@out" + i + ")",
          OutputFieldFragment.name(outputIndex) + "_partStride")
      }

      // New-style call (GPU operators)
      if (addressing == BigTensorAddressing) {
        if (lazyReplace("_writeTensorElementLocal" + i,
          FieldIO.writePointer(fieldTypes(i), OutputFieldFragment.name(outputIndex),
            fieldLocal = true, tensorLocal = true))) {
          if (outputIndex < 0)
            require(outputIndex >= 0, "Code gen failure: outputIndex not set.")
          result = result.replace("fieldName(@out" + i + ")",
            OutputFieldFragment.name(outputIndex))
          result = result.replace("partStride(@out" + i + ")",
            OutputFieldFragment.name(outputIndex) + "_partStride")
        }
      }
      else {
        // New-style call (GPU operators)
        if (lazyReplace("_writeTensorElementLocal" + i, name(i))) {
        }
      }

      /////////////////////////////////////////////////////////////////////////
      // Write tensor local
      /////////////////////////////////////////////////////////////////////////

      // Old-style call
      if (lazyReplace("@out" + i, name(i))) {
        require(addressing != BigTensorAddressing, "Illegal write")
      }

      // New-style call (GPU operators)
      if (lazyReplace("_writeTensorLocal" + i, name(i))) {
        // Is this requirement still needed?
        //require(addressing != BigTensorAddressing, "Illegal write")
      }

    }
    result
  }

  /** Inputs of this CodeFragment that are CodeFragments.
    *
    * @return Sequence of code fragments
    */
  def children = inputs

  override def toString =
    "CodeFragment " + name + "(" + inputs.map(_.toString).reduceLeft(_ + " " + _) + ")"
}
