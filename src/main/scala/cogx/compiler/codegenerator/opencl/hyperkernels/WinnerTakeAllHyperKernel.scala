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
import cogx.platform.types.ElementTypes.Float32
import cogx.compiler.codegenerator.opencl.fragments._
import WinnerTakeAllHyperKernel._
import HyperKernel._
import cogx.platform.opencl.OpenCLAbstractKernel
import cogx.cogmath.geometry.Shape
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.compiler.parser.op.{MaxPositionOp, WinnerTakeAllOp, WinnerTakeAllPointToFieldOp, WinnerTakeAllReduceOp}

/**
  * A suite of kernels that identify the largest value in a scalar field.  The
  * output can be an entire field, with the lowest-indexed max value set to 1
  * (as invoked by field.winnerTakeAll), or it can be a 0D vector field with the
  * coordinates of the max point (as invoked by field.maxPosition).
  *
  * WinnerTakeAll can be applied to a Vector or Matrix field, where the winner
  * is determined and set independently for each of the field slices.
  *
  * @author Dick Carter
  */

/** This kernel can run on 2D fields, and correctly avoids processing the
  * padding elements.  For 1D fields, it was easiest to use this same kernel, with
  * the localWorkSizeY set to 1
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
  * @param addressingMode The addressing mode of this kernel.
  * @param innerLoopLoads The number of values each thread reduces prior to
  *        a further reduction performed between threads
  * @param warpSize The number of threads that execute in lock-step without need for memory synchronization.
  *
  */
private[cogx]
class WinnerTakeAllReduceHyperKernel (in: Array[VirtualFieldRegister],
                                     operation: Opcode,
                                     addressingMode: AddressingMode,
                                     innerLoopLoads: Int,
                                     warpSize: Int)
        extends HyperKernel(operation, in,
          Array(outFieldType(inField2WorkField(in(0).fieldType, innerLoopLoads), addressingMode),
                outFieldType(inField2WorkField(in(0).fieldType, innerLoopLoads), addressingMode)),
          addressingMode)
{
  private val inFieldType = in(0).fieldType
  private val inDim = inFieldType.dimensions

  override lazy val workFieldType = inField2WorkField(inFieldType, innerLoopLoads)
  override lazy val workGroup = HyperKernel.computeWorkGroupParameters(workFieldType, addressing, localRows(workFieldType), localColumns(workFieldType))

  private val localWorkSize = workGroup.localWorkSize

  val firstPassKernel = in.length == 1

  /**Synthesize the OpenCL kernel code. */
  val code = new StringBuffer
  code append "#define firstIsWinner(a,b)  ((a).x > (b).x || ((a).x == (b).x && as_uint((a).y) <= as_uint((b).y)))\n"
  code append "#define winner(a,b)  ( firstIsWinner((a),(b)) ? (a) : (b))\n"

  code.append("        __local volatile float2 lMem[" + localWorkSize + "];\n")

  if (inDim > 2) {
    code.append("        const int layers = " + inFieldType.layers + ";\n")
    code.append("        const int startLayer = _layer;\n")
  }
  if (inDim > 1) {
    code.append("        const int rows = " + inFieldType.rows + ";\n")
    code.append("        const int startRow = _row;\n")
  }
  code.append("        const int columns = " + inFieldType.columns + ";\n")
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

  code.append("        lMem[threadIndex] = (float2) (-FLT_MAX, as_float(0));\n")
  code.append("\n")
  if (inDim > 2)
    code.append("        for (layer = startLayer; layer < layers; layer += _layers) {\n")
  if (inDim > 1)
    code.append("            for (row = startRow; row < rows; row += _rows) {\n")
  code.append("                for (column = startCol; column < columns; column += " + innerLoopLoads + " * _columns) {\n")

  if (firstPassKernel) {
    code.append("                    uint globalIndex = " + globalId(inDim, withUnderscore = false))
    code.append("                    float2 winnerCandidate = (float2) (readNonlocal(@in0), as_float(globalIndex));\n")
  }
  else
    code.append("                    float2 winnerCandidate = (float2) (readNonlocal(@in0), readNonlocal(@in1));\n")

  code.append("                    lMem[threadIndex] = winner(lMem[threadIndex], winnerCandidate);\n")
  for (i <- 0 until innerLoopLoads - 1) {
    code.append("                    column += _localColumns;\n")
    code.append("                    if (column < columns) {\n")
    if (firstPassKernel) {
      code.append("                        globalIndex += _localColumns;\n")
      code.append("                        winnerCandidate = (float2) (readNonlocal(@in0), as_float(globalIndex));\n")
    }
    else
      code.append("                        winnerCandidate = (float2) (readNonlocal(@in0), readNonlocal(@in1));\n")

    code.append("                        lMem[threadIndex] = winner(lMem[threadIndex], winnerCandidate);\n")
    code.append("                    }\n")
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
      code.append("            lMem[threadIndex] = winner(lMem[threadIndex], lMem[threadIndex + " + midPoint + "]);\n")
      if (midPoint > warpSize || midPoint == 1)
        code.append("        }\n")
      if (midPoint > warpSize)
        code.append("        barrier(CLK_LOCAL_MEM_FENCE);\n")
    }
    threadCount /= 2
  }
  code.append("\n")
  code.append("        if (threadIndex == 0) {\n")

  if (addressing == TensorElementAddressing) {
    val workGroupsPerTensorElement =
      workGroup.workGroups / workFieldType.tensorShape.points
    code.append("            column = groupId % " + workGroupsPerTensorElement + ";\n")
  }
  else
    code.append("            column = groupId;\n")
  code.append("            row = 0;\n")
  code.append("            @outNonlocal0 = lMem[0].x;\n")
  code.append("            @outNonlocal1 = lMem[0].y;\n")
  code.append("        }\n")

  code.append("#undef firstIsWinner\n")
  code.append("#undef winner\n")
  code.append("#undef floatAsUint\n")
  code.append("#undef uintAsFloat\n")

  addCode(code.toString)
}

/** The output of the tree of reduction kernels is a single-point field identifying the
  * index of the winner (with uint32s, masquerading as float32s).  This kernel
  * transforms this small input into a large field with the winner's position
  * marked with a 1f (all else 0f).
  *
  * The reason this is 1x1 is historical: one couldn't read in the length-2
  * vector as a float2 unless the field was considered a "tensor field," which
  * required it to be 2D.  This kernel-suite should now be revamped to work
  * with a 0D point field since the addressing limitation was lifted.
  *
  * @param in The input virtual field register driving this kernel.
  * @param operation The opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressingMode The addressing mode of this kernel.
  */
private[cogx]
class WinnerTakeAllPointToField (in: VirtualFieldRegister,
                                         operation: Opcode,
                                         resultType: FieldType,
                                         addressingMode: AddressingMode)
        extends HyperKernel(operation, Array(in), resultType, addressingMode)
{
  // The pointField is a 0D field with value being winner's global id
  val winningPointFieldType = in.fieldType
  require(winningPointFieldType.dimensions == 0, "point field argument must be a 0D field.")
  require(winningPointFieldType.tensorShape.dimensions <= 1, "point field argument must be a scalar or vector field.")


  val code = new StringBuffer
  code.append("    unsigned int global_id = " + globalId(fieldType.dimensions,
    withUnderscore = true))

  if (addressing == TensorElementAddressing)
    code.append("        tensorElement = _tensorElement;\n")
  defineAndSetLayerRowColumn(winningPointFieldType, "0", "0", "0")
  code.append("        unsigned int winnerId = as_uint((readNonlocal(@in0)));\n")
  code.append("        @out0 = (winnerId == global_id) ? 1.0f : 0.0f;")

  addCode(code.toString)
}

/** The output of the tree of reduction kernels is a single-point field identifying the
  * index of the winner (as a uint32s masquerading as a float32).  This kernel
  * transforms this small input into a 0D vector field with the vector elements
  * being the coordinates of the winner's position, now as floats.  Invoked by
  * the user function field.maxPosition().
  *
  * @param pointField The virtual field register of the input field describing
  * the index of the winner's position.
  * @param operation The opcode for this operation.
  * @param inType The FieldType of the original input field used to calculate
  *               the index of the winner's position.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressingMode The addressing mode of this kernel.
  */
private[cogx]
class WinnerTakeAllPointToPosition (pointField: VirtualFieldRegister,
                                         operation: Opcode,
                                         inType: FieldType,
                                         resultType: FieldType,
                                         addressingMode: AddressingMode)
        extends HyperKernel(operation, Array(pointField), resultType, addressingMode)
{
  // The pointField is a 0D field with value being winner's global index
  val winningPointFieldType = pointField.fieldType
  require(winningPointFieldType.dimensions == 0, "point field argument must be a 0D field.")
  require(winningPointFieldType.tensorShape.dimensions == 0, "point field argument must be a scalar.")

  private val inDim = inType.dimensions

  val code = new StringBuffer

  defineAndSetLayerRowColumn(winningPointFieldType, "0", "0", "0")
  code.append("        unsigned int winnerIndex = as_uint((readNonlocal(@in0)));\n")

  if (inDim >= 3) {
    val pointsPerLayer = inType.rows * inType.columns
    code.append("        unsigned int winnerLayer = winnerIndex / " + pointsPerLayer + ";\n")
    code.append("        winnerIndex -= winnerLayer * " + pointsPerLayer + ";\n")
  }

  if (inDim >= 2) {
    code.append("        unsigned int winnerRow = winnerIndex / " + inType.columns + ";\n")
    code.append("        winnerIndex -= winnerRow * " + inType.columns + ";\n")
  }

  code.append("        unsigned int winnerColumn = winnerIndex;\n")

  inDim match {
    case 3 => code.append("        @out0 = (float3) (winnerLayer, winnerRow, winnerColumn);")
    case 2 => code.append("        @out0 = (float2) (winnerRow, winnerColumn);")
    case 1 => code.append("        @out0 = (float) winnerColumn;")
    case 0 => code.append("        @out0 = (float) winnerColumn;")
  }

  addCode(code.toString)
}

/** Factory object to create the chain of kernels to reduce the input to 1 value.
  */
private[cogx]
object WinnerTakeAllHyperKernel {

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

  private def reduceToPoint(inputs: Array[VirtualFieldRegister], addressing: AddressingMode, warpSize: Int): VirtualFieldRegister = {
    
    if (inputs.length == 2 &&
        inputs(0).fieldType.fieldShape.points == 1)
      inputs(1)
    else {
      val thisPassKernel = new WinnerTakeAllReduceHyperKernel(inputs, WinnerTakeAllReduceOp, addressing, InnerLoopLoads, warpSize)
      reduceToPoint(thisPassKernel.outputs.toArray, addressing, warpSize)
    }
  }

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
  def apply(input: VirtualFieldRegister, operation: Opcode, resultType: FieldType, warpSize: Int): OpenCLAbstractKernel = {

    val inType = input.fieldType
    val expectedResultType =
      operation match {
        case WinnerTakeAllOp =>
          inType
        case MaxPositionOp =>
          require(isTensor0Field(inType))
          new FieldType(Shape(), Shape(inType.dimensions), Float32)
        case _ =>
          throw new RuntimeException("Unexpected operation " + operation + ", expecting WinnerTakeAllOp or MaxPositionOp")
      }
    require(expectedResultType == resultType)

    val addressing =
      if (isTensor0Field(inType))
        SmallTensorAddressing
      else
        TensorElementAddressing

    val winningPoint = reduceToPoint(Array(input), addressing, warpSize)

    operation match {
      case WinnerTakeAllOp =>
        new WinnerTakeAllPointToField(winningPoint, WinnerTakeAllPointToFieldOp, resultType, addressing)
      case MaxPositionOp =>
        new WinnerTakeAllPointToPosition(winningPoint, MaxPositionOp, input.fieldType, resultType, addressing)
      case _ =>
        throw new RuntimeException("Unexpected operation " + operation + ", expecting WinnerTakeAllOp or MaxPositionOp")
    }
  }

  /** If the inner loop of the kernel does multiple loads, then the apparent work field is
    * smaller than input field in the columns direction
    */
  def inField2WorkField(inFieldType: FieldType, innerLoopLoads: Int): FieldType = {
    val newColumns = (HyperKernel.roundUp(innerLoopLoads,inFieldType.columns) / innerLoopLoads).toInt
    val workFieldShape = inFieldType.dimensions match {
      case 3 => Shape(inFieldType.layers, inFieldType.rows, newColumns)
      case 2 => Shape(inFieldType.rows, newColumns)
      case 1 => Shape(newColumns)
      case x => throw new RuntimeException("Illegal dimensions: " + x)
    }

    new FieldType(workFieldShape, inFieldType.tensorShape, inFieldType.elementType)
  }

  
  def outFieldType(workFieldType: FieldType, addressing: AddressingMode): FieldType = {
    val workGroups = HyperKernel.computeWorkGroupParameters(workFieldType, addressing, localRows(workFieldType), localColumns(workFieldType)).workGroups

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

  def globalId(inDim: Int, withUnderscore: Boolean) = {
    val underscore = if (withUnderscore) "_"  else ""
    val result = inDim match {
      case 0 => "0;\n"
      case 1 => "%u%column;\n"
      case 2 => "%u%row * %u%columns + %u%column;\n"
      case 3 => "%u%layer * %u%rows * %u%columns + %u%row * %u%columns + %u%column;\n"
      case x => throw new RuntimeException("Unsupported input field dimension: " + x)
    }
    result.replaceAll("%u%", underscore)
  }
}
