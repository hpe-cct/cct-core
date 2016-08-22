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

import cogx.cogmath.geometry.Shape
import cogx.compiler.codegenerator.opencl.fragments.{AddressingMode, HyperKernel, TensorElementAddressing}
import cogx.compiler.codegenerator.opencl.fragments.HyperKernel._
import cogx.platform.types._
import cogx.compiler.parser.op.FilterAdjointShuffleOp

/** Hyperkernel that reorders the planes of a vector field that is holding a batch
  * of multi-feature-plane images.  The input is assumed to have planes associated
  * with a given image in adjacent planes, while the output is created so that planes
  * of the same feature are adjacent.
  *
  * Input:                    Output:
  *
  * Image_0_feature_0         Image_0_feature_0
  * Image_0_feature_1         Image_1_feature_0
  * ...                       ...
  * Image_0_feature_N-1       Image_B-1_feature_0
  * Image_1_feature_0         Image_0_feature_1
  * ...                       ...
  *
  * @author Dick Carter
  *
  * @param in The virtual field register of the input field to be flipped.
  * @param operation The FilterAdjointShuffle Opcode.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class FilterAdjointShuffleHyperKernel private (in: Array[VirtualFieldRegister],
                                        operation: FilterAdjointShuffleOp,
                                        resultType: FieldType,
                                        addressMode: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressMode) {
  import operation._
  val inType = in(0).fieldType
  require(inType.tensorColumns % batchSize == 0,
    s"Internal error: Expecting VectorField tensor depth to be a multiple of $batchSize, found fieldType $inType.")
  val featuresPerImage = inType.tensorShape.points / batchSize
  val numimages = untilImage - fromImage

  val code =
    s"""|        int featureIndex = _tensorElement / $numimages;
        |        int imageIndex = $fromImage + _tensorElement % $numimages;
        |        tensorElement = imageIndex * $featuresPerImage + featureIndex;
        |${setLayerRowColumn(resultType, "_layer", "_row", "_column")}
        |        @out0 = readElementNonlocal(@in0);
        |""".stripMargin

  addCode(code)
  //debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object FilterAdjointShuffleHyperKernel {
  /** Create a hyperkernel that flips a tensor field.
    *
    * @param in The virtual field register of the input field to be flipped.
    * @param operation The FlipOp opcode.
    * @param resultType The FieldType of the result of this kernel.
    * @return The synthesized hyperkernel.
    */
  def apply(in: Array[VirtualFieldRegister], operation: FilterAdjointShuffleOp, resultType: FieldType):
  HyperKernel =
  {
    import operation._
    require(in.length == 1, s"Expecting a single input, found ${in.length}.")
    val inType = in(0).fieldType
    val numImages = untilImage - fromImage
    require(inType.tensorColumns % batchSize == 0,
      s"Internal error: Expecting VectorField tensor depth to be a multiple of $batchSize, found fieldType $inType.")
    val featuresPerImage = inType.tensorShape.points / batchSize
    val expectedResultType = inType.resizeTensor(Shape(featuresPerImage * numImages))
    require(expectedResultType == resultType, s"Internal error: Expecting $expectedResultType == $resultType.")
    require(inType.tensorOrder == 1, s"Internal error: Expecting VectorField input, found $inType.")

    new FilterAdjointShuffleHyperKernel(in, operation, resultType, TensorElementAddressing)
  }
}

