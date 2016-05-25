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

import cogx.compiler.codegenerator.opencl.fragments._
import cogx.platform.types._
import Median2D_3x3HyperKernel._
import cogx.compiler.parser.op.MedianFilterOp

/** 3 x 3 Median filter of a 2D scalar field.
  *
  * @author Greg Snider and Dick Carter
  *
  * @param in The input virtual field register driving this kernel.
  * @param operation The binary opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  */
private[cogx]
class Median2D_3x3HyperKernel private (in: VirtualFieldRegister,
                                            operation: Opcode,
                                            resultType: FieldType)
        extends HyperKernel(operation, Array(in), resultType, SmallTensorAddressing)
{
  val inType = in.fieldType
  val code = new StringBuffer
  // First step is to read in a tile of the input image into local memory.
  val localTile = LocalTensorMemory2D(inType.fieldShape, CLFloat,
    topHalo = 1, rightHalo = 1, bottomHalo = 1, leftHalo = 1,
    borderProcessing = BorderClamp)
  code.append(localTile)
  // Image tile is cached, so do the filtering. Pull out the appropriate
  // 3 x 3 aperture (window) for this thread.  I verified that the aperture
  // array is compiled away by NVidia's compiler (not present in PTX assembly).  -RJC
  code append "    float aperture[9];\n"
  for (row <- 0 until 3; col <- 0 until 3)
    code append "    aperture[" + (3*row+col) + "] = " +
                "localImage[_localRow + " + row + "][_localColumn + " + col + "];\n"

  // Call Intel's median filter code that uses bitonic sort. This takes
  // "aperture" as an input and returns solution in "result".
  code.append(medianFilterCode)

  code append "    @out0 = result;\n"
  addCode(code.toString)

  //debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object Median2D_3x3HyperKernel {

  /** Create a hyperkernel that performs a 3 x 3 Median filtering of a 2D scalar field.
    *
    * @param in The input virtual field register driving this kernel.
    * @param operation The binary opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    */
  def apply(in: VirtualFieldRegister, operation: Opcode, resultType: FieldType): AbstractKernel =
  {
    val inType = in.fieldType

    require(inType.dimensions == 2)
    require(inType.tensorShape.dimensions == 0)
    require(inType == resultType)
    require(operation == MedianFilterOp)
    new Median2D_3x3HyperKernel(in, operation, resultType)
  }

  /**
   * Median filter code from Intel. This expects an input:
   * <pre>
   *   float aperture[9];
   * </pre>
   * that has been loaded with the 9 values of the 3x3 window to be filtered.
   * It returns the median-filtered resulting pixel in:
   * <pre>
   *   float result;
   * </pre>
   */
  private def medianFilterCode: String = {
    val code = new StringBuilder
    code.append("float r0,r1,r2,r3,r4,r5,r6,r7,r8;\n")
    code.append("r0=aperture[0];\n")
    code.append("r1=aperture[1];\n")
    code.append("r2=aperture[2];\n")
    code.append("r3=aperture[3];\n")
    code.append("r4=aperture[4];\n")
    code.append("r5=aperture[5];\n")
    code.append("r6=aperture[6];\n")
    code.append("r7=aperture[7];\n")
    code.append("r8=aperture[8];\n")

    // perform partial bitonic sort to find current channel median
    code.append("float smallest = min(r0, r1);\n")
    code.append("float biggest = max(r0, r1);\n")
    code.append("r0 = smallest;\n")
    code.append("r1 = biggest;\n")
    code.append("\n")
    code.append("smallest = min(r3, r2);\n")
    code.append("biggest = max(r3, r2);\n")
    code.append("r3 = smallest;\n")
    code.append("r2 = biggest;\n")

    code.append("smallest = min(r2, r0);\n")
    code.append("biggest = max(r2, r0);\n")
    code.append("r2 = smallest;\n")
    code.append("r0 = biggest;\n")

    code.append("smallest = min(r3, r1);\n")
    code.append("biggest = max(r3, r1);\n")
    code.append("r3 = smallest;\n")
    code.append("r1 = biggest;\n")

    code.append("smallest = min(r1, r0);\n")
    code.append("biggest = max(r1, r0);\n")
    code.append("r1 = smallest;\n")
    code.append("r0 = biggest;\n")

    code.append("smallest = min(r3, r2);\n")
    code.append("biggest = max(r3, r2);\n")
    code.append("r3 = smallest;\n")
    code.append("r2 = biggest;\n")

    code.append("smallest = min(r5, r4);\n")
    code.append("biggest = max(r5, r4);\n")
    code.append("r5 = smallest;\n")
    code.append("r4 = biggest;\n")

    code.append("smallest = min(r7, r8);\n")
    code.append("biggest = max(r7, r8);\n")
    code.append("r7 = smallest;\n")
    code.append("r8 = biggest;\n")

    code.append("smallest = min(r6, r8);\n")
    code.append("biggest = max(r6, r8);\n")
    code.append("r6 = smallest;\n")
    code.append("r8 = biggest;\n")

    code.append("smallest = min(r6, r7);\n")
    code.append("biggest = max(r6, r7);\n")
    code.append("r6 = smallest;\n")
    code.append("r7 = biggest;\n")

    code.append("smallest = min(r4, r8);\n")
    code.append("biggest = max(r4, r8);\n")
    code.append("r4 = smallest;\n")
    code.append("r8 = biggest;\n")

    code.append("smallest = min(r4, r6);\n")
    code.append("biggest = max(r4, r6);\n")
    code.append("r4 = smallest;\n")
    code.append("r6 = biggest;\n")

    code.append("smallest = min(r5, r7);\n")
    code.append("biggest = max(r5, r7);\n")
    code.append("r5 = smallest;\n")
    code.append("r7 = biggest;\n")

    code.append("smallest = min(r4, r5);\n")
    code.append("biggest = max(r4, r5);\n")
    code.append("r4 = smallest;\n")
    code.append("r5 = biggest;\n")

    code.append("smallest = min(r6, r7);\n")
    code.append("biggest = max(r6, r7);\n")
    code.append("r6 = smallest;\n")
    code.append("r7 = biggest;\n")

    code.append("smallest = min(r0, r8);\n")
    code.append("biggest = max(r0, r8);\n")
    code.append("r0 = smallest;\n")
    code.append("r8 = biggest;\n")

    code.append("r4 = max(r0, r4);\n")
    code.append("r5 = max(r1, r5);\n")

    code.append("r6 = max(r2, r6);\n")
    code.append("r7 = max(r3, r7);\n")

    code.append("r4 = min(r4, r6);\n")
    code.append("r5 = min(r5, r7);\n")

    //store found median into result
    code.append("float result = min(r4, r5);\n")
    code.toString()
  }

}

