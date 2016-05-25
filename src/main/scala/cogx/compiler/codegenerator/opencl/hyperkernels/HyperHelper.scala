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

import cogx.compiler.codegenerator.opencl.fragments.{TensorElementAddressing, SmallTensorAddressing, AddressingMode}
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.platform.types.{VirtualFieldRegister, FieldType}


/** Functions that make life easier for HyperKernel factories.
  *
  * @author Greg Snider
  */
private[cogx]
trait HyperHelper {

  /** Deduce the best addressing mode for a HyperKernel with a given set of
    * inputs and result type. FieldPointAddressing is preferred since it
    * can result in more kernel merging.
    *
    * @param inputs Inputs to the hyperkernel.
    * @param result Result type of the hyperkernel.
    * @return Best addressing mode.
    */
  def bestAddressMode(inputs: Array[VirtualFieldRegister], result: FieldType): AddressingMode = {
    var inputsAreTensorFields =
      inputs.map(f => isSmallTensorField(f.fieldType)).reduceLeft(_ && _)
    if (inputsAreTensorFields && isSmallTensorField(result))
      SmallTensorAddressing
    else
      TensorElementAddressing
  }
}