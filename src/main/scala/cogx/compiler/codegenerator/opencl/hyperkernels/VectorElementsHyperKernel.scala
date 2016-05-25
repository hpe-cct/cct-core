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

import cogx.platform.types.{VirtualFieldRegister, FieldType}
import cogx.compiler.codegenerator.opencl.fragments.{TensorElementAddressing, HyperKernel}
import cogx.compiler.parser.op.VectorElementsOp


/** Dynamically indexes a vector field
  * 
  * @author Matthew Pickett
  */
private[cogx]
class VectorElementsHyperKernel(in:Array[VirtualFieldRegister], resultType:FieldType)
  extends HyperKernel(VectorElementsOp, in, resultType, TensorElementAddressing){

  val indicesIs0D = in(1).fieldType.dimensions == 0
  val maxIndex = in(0).fieldType.tensorShape(0)-1
  val lrcCode = fieldType.dimensions match {
    case 0 => ""
    case 1=> "int column = _column;"
    case 2=> "int column = _column; int row = _row;"
    case 3=> "int column = _column; int row = _row; int layer = _layer;"
    case _ => throw new RuntimeException("Invalid number of field dimensions")
  }

  val indicesFieldIndexCode = if(indicesIs0D) "" else lrcCode
  val inputFieldIndexCode = if(indicesIs0D) lrcCode else ""

  val code =
    s"""
      |  $indicesFieldIndexCode
      |  int tensorElement = _tensorElement;
      |  tensorElement = readElementNonlocal(@in1);
      |  $inputFieldIndexCode
      |
      |  float e;
      |  if(tensorElement > $maxIndex || tensorElement < 0) { e = NAN;}
      |  else {e = readElementNonlocal(@in0);}
      |  @out0 = e;
      |
    """.stripMargin
  addCode(code)
}
