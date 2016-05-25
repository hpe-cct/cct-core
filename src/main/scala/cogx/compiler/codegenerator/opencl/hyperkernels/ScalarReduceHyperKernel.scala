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

package cogx.compiler.codegenerator.opencl.hyperkernels

import cogx.platform.types._
import cogx.compiler.codegenerator.opencl.fragments._
import ScalarReduceHyperKernel._
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.cogmath.geometry.Shape
import cogx.compiler.codegenerator.opencl.OpcodeToFunction
import cogx.compiler.parser.op.{FieldReduceMedianOp, FieldReduceMaxOp, FieldReduceMinOp, FieldReduceSumOp}

/** Reduces the points of a field (i.e. scalars, tensors) to a single point
  * based on a reduction operation (e.g. sum, min, max).
  *
  * This kernel can run on 2D fields, and correctly avoids processing the
  * padding elements.  For 1D fields, it was easiest to use this same kernel, with
  * the localWorkSizeY set to 1.
  *
  * The kernel produces one value for each workgroup launched.  If only a single
  * workgroup is launched, then the output is a 0D ScalarField.  If multiple
  * workgroups are launched, then the output is a multi-element 1D ScalarField.
  *
  * The ScalarReduceKernel factory method controls creating the chain of kernels
  * needed to reduce the input field down to one value.
  *
  * The kernel is written to step a smaller workField shape over a larger input
  * shape (in all 3 dimensions).  Currently only the column dimension of the
  * workField is made smaller relative to the input, by the 'innerLoopLoads'
  * factor, as a 25-85% speed-up was seen with this approach.  Note that the
  * input size is set up by the 'layers', 'rows' and 'columns' variables, while
  * the workField size is specified by '_layers', '_rows' and '_columns'
  *
  * TODO: Follow-on work may improve the precision of the result when the
  * operation is reduceSum.  Doing the accumulation with doubles is one option
  * (but the GK104 is DP-unit starved).  Using Knuth's "two-sum" algorithm
  * is another option, which might have less impact than one might think if the
  * kernels are memory bandwidth limited.
  *
  * @author Dick Carter
  *
  * @param in The input virtual field register driving this kernel.
  * @param operation The opcode for this operation.
  * @param addressing The addressing mode of this kernel.
  * @param innerLoopLoads The number of values each thread reduces prior to.
  * @param warpSize The number of threads that execute in lock-step without need for memory synchronization.
  */
private[cogx]
class ScalarReduceHyperKernel private (in: Array[VirtualFieldRegister], operation: Opcode, addressing: AddressingMode, innerLoopLoads: Int, warpSize: Int)
        extends HyperKernel(operation, in, outFieldType(inField2WorkField(in(0).fieldType,innerLoopLoads), addressing), addressing)
{
  val inFieldType = in(0).fieldType
  private val inDim = inFieldType.dimensions

  override lazy val workFieldType = inField2WorkField(in(0).fieldType, innerLoopLoads)
  override lazy val workGroup = HyperKernel.computeWorkGroupParameters(workFieldType, addressing, localRows(workFieldType), localColumns(workFieldType))

  private val localWorkSize = workGroup.localWorkSize

  val readType = addressing.clType(fieldType).name
  val readZero = addressing.clType(fieldType).zero

  /** Synthesize the OpenCL kernel code. */
  val code = new StringBuffer
  /** Macro to invoke to perform reduction, e.g. FieldReduceSum => "add" */
  val reductionOp = OpcodeToFunction(operation)
  /** This kernel is not as extensible as we'd like: we still need to hard-code
    * the initial 'zero' element.  Might be avoided by initializing to the
    * first element read.  Could also provide zero from the opcode.
    */
  val initVal = operation match {
    case FieldReduceSumOp => readZero
    case FieldReduceMinOp => "((" + readType + ") FLT_MAX)"
    case FieldReduceMaxOp => "((" + readType + ") -FLT_MAX)"
    case FieldReduceMedianOp => throw new RuntimeException("median not supported by this kernel")
    case x => throw new RuntimeException("unknown op: " + x)
  }

  if (inDim == 0) {
    code.append("@out0 = read(@in0);\n")
  }
  else {
    code.append("        __local volatile " + readType + " lMem[" + localWorkSize + "];\n")

    if (inDim > 2) {
      code.append("        const int layers = " + inFieldType.layers + ";\n")
      code.append("        const int startLayer = _layer;\n")
    }
    if (inDim > 1) {
      code.append("        const int rows = " + inFieldType.rows + ";\n")
      code.append("        const int startRow = _row;\n")
    }
    code.append("        const int cols = " + inFieldType.columns + ";\n")
    code.append("        const int startCol = _column + " + (innerLoopLoads-1) + " * _localColumns * get_group_id(0);\n")
    code.append("        const unsigned int threadIndex = _localColumn")
    if (inDim > 1)
      code.append(" + _localRow * _localColumns")
    if (inDim > 2)
      code.append(" + _localLayer * _localColumns * _localRows")
    code.append(";\n")
    if (addressing == TensorElementAddressing)
      code.append("        tensorElement = _tensorElement;\n")
    code.append("        const unsigned int groupId = get_group_id(0)")
    if (inDim > 1)
      code.append(" + get_group_id(1) * get_num_groups(0)")
    if (inDim > 2)
      code.append(" + get_group_id(2) * get_num_groups(1) * get_num_groups(0)")
    code.append(";\n")

    code.append("        lMem[threadIndex] = " + initVal + ";\n")
    code.append("\n")
    if (inDim > 2)
      code.append("        for (layer = startLayer; layer < layers; layer += _layers) {\n")
    if (inDim > 1)
      code.append("            for (row = startRow; row < rows; row += _rows) {\n")
    code.append("                for (column = startCol; column < cols; column += " + innerLoopLoads + " * _columns) {\n")
    code.append("                    lMem[threadIndex] = " + reductionOp + "(lMem[threadIndex], readNonlocal(@in0));\n")
    for (i <- 0 until innerLoopLoads - 1) {
      code.append("                    column += _localColumns;\n")
      code.append("                    if (column < cols) \n")
      code.append("                        lMem[threadIndex] = " + reductionOp + "(lMem[threadIndex], readNonlocal(@in0));\n")
    }
    code.append("                } \n")
    if (inDim > 1)
      code.append("            } \n")
    if (inDim > 2)
      code.append("        } \n")
    code.append("\n")
    code.append("        barrier(CLK_LOCAL_MEM_FENCE);\n")
    code.append("\n")

    var threadCount = 1024
    while (threadCount >= 2) {
      val midPoint = threadCount / 2
      if (localWorkSize >= threadCount) {
        if (midPoint >= warpSize)
          code.append("        if (threadIndex < " + midPoint + ") {\n")
        code.append("            lMem[threadIndex] = " + reductionOp + "(lMem[threadIndex], lMem[threadIndex + " + midPoint + "]);\n")
        if (midPoint > warpSize || midPoint == 1)
          code.append("        }\n")
        if (midPoint > warpSize)
          code.append("        barrier(CLK_LOCAL_MEM_FENCE);\n")
      }
      threadCount /= 2
    }
    code.append("\n")

    def toHexChar(i: Int) = {
      require(i >= 0 && i < 16)
      "0123456789ABCDEF"(i)
    }

    // Write out the output as a non-local write.  This is handled in-line if the value is a simple scalar (not a
    // float2, float3, etc.), so we use @outElementNonlocal where needed to avoid vector writes.
    //
    // Because the write is in-lined, it need not be the last statement.  By including the output write in the
    // "if (threadIndex == 0) { ... }" statement, we avoid the thread return statement that kills mergeability of this kernel.

    code.append("        if (threadIndex == 0) {\n")
    addressing match {
      case SmallTensorAddressing =>
        code.append("        column = groupId;\n")
        if (isTensor0Field(fieldType))
          code.append("        @outNonlocal0 = lMem[0];\n")
        else {
          for(i <- 0 until fieldType.tensorShape.points) {
            code.append("        tensorElement = " + i + ";\n")
            if (fieldType.tensorShape.points == 1)
              code.append("        @outElementNonlocal0 = lMem[0];\n")
            else
              code.append("        @outElementNonlocal0 = lMem[0].s" + toHexChar(i) + ";\n")
          }
        }
      case TensorElementAddressing =>
        val workGroupsPerTensorElement =
          workGroup.workGroups / workFieldType.tensorShape.points
        code.append("        column = groupId % " + workGroupsPerTensorElement + ";\n")
        code.append("        @outNonlocal0 = lMem[0];\n")
      case BigTensorAddressing =>
        throw new RuntimeException("unexpected address mode")
    }
    code.append("        }\n")
  }

  addCode(code.toString)
}

/**
 * Factory object to create the chain of kernels to reduce the input to 1 value.
 */
private[cogx]
object ScalarReduceHyperKernel {

  val InnerLoopLoads = 2

  // Currently the NonlocalOperator trait protects this kernel from being
  // merged into other kernels that may have a different local workgroup
  // size.  When the kernel merging becomes more sophisticated, it must watch out
  // for kernels like this one that change the localRows and localColumns.
  val LocalRows = 2
  val LocalColumns = 128

  // The following adjustment to LocalRows and LocalColumns is in an attempt
  // to do the reduction in one kernel level if possible.  For example, a 16x16
  // input would otherwise be 2 levels if a 2x128 kernel was used.  BTW, 2x128
  // was chosen as the highest-performing workgroup size for large inputs.

  private def localRowsColumns(workFieldType: FieldType) = {
    val workColumns = workFieldType.columns
    val workRows = workFieldType.rows

    var localColumns = LocalColumns
    var localRows = LocalRows


    while(localColumns/2 >= workColumns && localRows < workRows && localColumns > 1) {
      localColumns /= 2
      localRows *= 2
    }
    (localRows, localColumns)
  }

  def localRows(workFieldType: FieldType) = localRowsColumns(workFieldType)._1
  def localColumns(workFieldType: FieldType) = localRowsColumns(workFieldType)._2

  /** Create a chain of Hyperkernels that reduce the input to a single point.
    *
    * This recursive factory method creates a chain of ScalarReduceKernels.
    * If invoked on a 1024x1024 input with a 2x128 localWorkgroup size, then
    * there would be a 256x8 "grid" of workgroups that would create a 2048
    * element 1D ScalarField as an output (remember that each thread processes
    * 2 values in its inner loop).  This would be fed into a second
    * kernel with a 1x256 localWorkgroup size, to generate a 4-long 1D field,
    * which would be reduced to a single value in a last pass.
    *
    * @param input The input virtual field register driving this kernel.
    * @param operation The opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    * @param warpSize The number of threads that execute in lock-step without need for memory synchronization.
    * @return Synthesized hyperkernel for the operation.
    */
  def apply(input: VirtualFieldRegister, operation: Opcode, resultType: FieldType, warpSize: Int): AbstractKernel = {

    val inType = input.fieldType
    require(to0D(inType) == resultType)
    val addressing =
      if (isSmallTensorField(inType))
        SmallTensorAddressing
      else
        TensorElementAddressing
    val thisPassKernel = new ScalarReduceHyperKernel(Array(input), operation, addressing, InnerLoopLoads, warpSize)

    if (thisPassKernel.fieldType.fieldShape.dimensions == 0)
      thisPassKernel
    else
      ScalarReduceHyperKernel(thisPassKernel.outputs(0), operation, resultType, warpSize)
  }

  /** If the inner loop of the kernel does multiple loads, then the apparent work field is
    * smaller than input field in the columns direction
    *
    */
  private def inField2WorkField(inFieldType: FieldType, innerLoopLoads: Int): FieldType = {
    val newColumns = (HyperKernel.roundUp(innerLoopLoads,inFieldType.columns) / innerLoopLoads).toInt
    val workFieldShape = inFieldType.dimensions match {
      case 3 => Shape(inFieldType.layers, inFieldType.rows, newColumns)
      case 2 => Shape(inFieldType.rows, newColumns)
      case 1 => Shape(newColumns)
      case 0 => Shape()
      case x => throw new RuntimeException("Illegal dimensions: " + x)
    }

    new FieldType(workFieldShape, inFieldType.tensorShape, inFieldType.elementType)
  }

  private def outFieldType(workFieldType: FieldType, addressing: AddressingMode): FieldType = {
    val workGroups = HyperKernel.computeWorkGroupParameters(workFieldType, addressing,
      localRows(workFieldType), localColumns(workFieldType)).workGroups

    addressing match {
      case SmallTensorAddressing =>
        if (workGroups == 1)
          new FieldType(Shape(), workFieldType.tensorShape, workFieldType.elementType)
        else
          new FieldType(Shape(workGroups), workFieldType.tensorShape, workFieldType.elementType)
      case TensorElementAddressing =>
        val workGroupsPerTensorElement = workGroups / workFieldType.tensorShape.points
        if (workFieldType.dimensions == 0 || workGroupsPerTensorElement == 1)
          new FieldType(Shape(), workFieldType.tensorShape, workFieldType.elementType)
        else
          new FieldType(Shape(workGroupsPerTensorElement), workFieldType.tensorShape, workFieldType.elementType)
      case BigTensorAddressing =>
        throw new RuntimeException("not implemented yet")
    }
  }
}
