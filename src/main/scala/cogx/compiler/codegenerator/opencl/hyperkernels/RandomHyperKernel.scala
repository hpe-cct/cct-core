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

import cogx.platform.types.VirtualFieldRegister
import cogx.compiler.codegenerator.opencl.fragments.{TensorElementAddressing, HyperKernel}
import cogx.compiler.parser.op.RandomOp

/** Implementation based on HP Tech rerport: 'FPGA implementation of
  * neighborhood-of-four cellular automata random number generators' by
  * Shackleford et al. (2001)
  *
  * Regards each point in the field as a cell and calculates a new state based on
  * the update rule outlined in the above reference.
  *
  * @author Matthew Pickett
  *
  * @param in The input virtual field register driving this kernel.
  * @param op The opcode describing the operation (should be RandomOp).
  */
private[cogx]
class RandomHyperKernel private (in:VirtualFieldRegister, op:RandomOp)
  extends HyperKernel(op, Array(in), in.fieldType, TensorElementAddressing)
{


  val bits = op.bits
  val mult = math.pow(2,bits).toInt - 1

  //Calcualte the lookup table used for the update rule. pick the first rule that
  // passed the Diehard tests, according to the above ref
  val CArule = 25946
  val bitSeq = Seq.tabulate(16){
    (i) => math.signum(CArule & math.pow(2,i).toInt)
  }
  val bitSeqString = bitSeq.map(_+",").reduce(_ + _).dropRight(1)

  val inType = in.fieldType
  val points = in.fieldType.fieldShape.points*in.fieldType.tensorShape.points
  val fieldDims = in.fieldType.fieldShape.dimensions

  val rows = math.max(inType.rows,1)
  val columns = math.max(inType.columns, 1)
  val layers = math.max(inType.layers, 1)
  val rowElems = math.max(inType.tensorRows, 1)
  val columnElems = math.max(inType.tensorColumns, 1)
  val elems = rowElems*columnElems
  val rowFactor = columns*layers*elems
  val columnFactor = layers*elems
  val layerFactor = elems

  // The type for the element
  val eType = "uint"

  eType match {
    case "short" => require(bits <= 15)
    case "ushort" => require(bits <= 16)
    case _ => require(bits <= 24)
  }

  //Add the lookup table as an array
  val code = new StringBuffer
  code append s"float bitSeq[16] = {$bitSeqString};"

  //calculate the index of current point, regarding field as a flat block of floats
  val indexCode = fieldDims match {
    case 0 => "int index = _tensorElement;"
    case 1 => s"int index = _column * $columnFactor +  _tensorElement;"
    case 2 => s"int index = _row * $rowFactor + _column * $columnFactor + _tensorElement;"
    case 3 => s"int index = _row * $rowFactor + _column * $columnFactor + _layer * $layerFactor + _tensorElement;"
    case _ => throw new RuntimeException("incorrect number of field dimensions")
  }
  code append indexCode

  //read in the current point
  code append s"$eType e0 = readElement(@in0) * $mult;"

  //read in the nonlocal points as a ushort
  code append "int curIndex = 0;"
  code append "int row = 0;"
  code append "int column = 0;"
  code append "int layer = 0;"
  code append "int tensorElement = 0;"
  def readCode(offset:Int, name:String) = {
    code append s"""
        | curIndex = (index + $offset + $points)%$points;
        | row = curIndex/$rowFactor;
        | column = (curIndex - row*$rowFactor)/$columnFactor;
        | layer = (curIndex - row*$rowFactor - column*$columnFactor)/$layerFactor;
        | tensorElement = curIndex - row*$rowFactor - column*$columnFactor - layer*$layerFactor;
        | $eType $name = readElementNonlocal(@in0) * $mult;
      """.stripMargin
  }

  readCode(-7, "en7")
  readCode(11, "e11")
  readCode(17, "e17")

  //for each bit in the ushort, evalutate the lookup table and accumulate into
  // the final output int
  code append s"""
    | float accum = 0;
    | for(int i =0; i < $bits; i++){
    |   $eType curn7 = ((en7 >> i) & 1) != 0;
    |   $eType cur0 = ((e0 >> i) & 1) != 0;
    |   $eType cur11 = ((e11 >> i) & 1) != 0;
    |   $eType cur17 = ((e17 >> i) & 1) != 0;
    |   $eType curSeqIndex = 8*curn7+4*cur0+2*cur11+cur17;
    |   float two = 2;
    |   float curPow = pown(two,i);
    |   accum += bitSeq[curSeqIndex] * curPow;
    | }
  """.stripMargin

//  code append "int seqIndex = en7+2*e0+4*e11+8*e17;"
  code append s"@out0 = accum/$mult;"
  addCode(code.toString)
}

/** Implementation based on HP Tech rerport: 'FPGA implementation of
  * neighborhood-of-four cellular automata random number generators' by
  * Shackleford et al. (2001)
  *
  * Regards each point in the field as a cell and calculates a new state based on
  * the update rule outlined in the above reference.
  *
  * @author Matthew Pickett
  *
  */
private[cogx]
object RandomHyperKernel{
  /**
    * @param in The input virtual field register driving this kernel.
    * @param op The opcode describing the operation (should be RandomOp).
    * @return
    */
  def apply(in:VirtualFieldRegister, op:RandomOp):HyperKernel =
    new RandomHyperKernel(in, op)

}
