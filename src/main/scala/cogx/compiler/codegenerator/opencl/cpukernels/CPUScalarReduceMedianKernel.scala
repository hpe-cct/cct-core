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

package cogx.compiler.codegenerator.opencl.cpukernels

import cogx.platform.types.ElementTypes.Float32
import cogx.platform.types.KernelTypes.{KernelType, CPUScalarReduceMedianKernelType}
import cogx.platform.types._
import cogx.compiler.parser.op._
import cogx.cogmath.geometry.Shape
import scala.util.Random
import cogx.platform.opencl.{OpenCLCpuSingleOutputKernel, OpenCLFieldRegister}
import cogx.platform.cpumemory.ScalarFieldMemory

/** Reduces a scalar field to a 0-dimensional scalar field containing the median
  * of the field values.
  *
  * @author Greg Snider and Dick Carter
  */
private[cogx]
class CPUScalarReduceMedianKernel(in: VirtualFieldRegister, op: Opcode, resultType: FieldType)
        extends OpenCLCpuSingleOutputKernel(op, Array(in), resultType)
{
  /** The type of the kernel, either DeviceKernel, or one of a number of CPU kernel types. */
  override val kernelType: KernelType = CPUScalarReduceMedianKernelType

  /** Flag to enable median approximation above a certain field size */
  val EnableApproximateMedian = true
  /**  field size threshold that triggers sampling strategy, odd is good */
  val ApproxMedianEnableThreshold = 999
  val inDim = in.fieldType.dimensions
  lazy val rand = new Random
  /** precalculated "random" positions in the field to sample for median (done
    * for performance reasons). */
  lazy val columnIndex = Array.tabulate(ApproxMedianEnableThreshold) {
    i => rand.nextInt(in.fieldType.columns)
  }
  lazy val rowIndex = Array.tabulate(ApproxMedianEnableThreshold) {
    i => rand.nextInt(in.fieldType.rows)
  }
  lazy val layerIndex = Array.tabulate(ApproxMedianEnableThreshold) {
    i => rand.nextInt(in.fieldType.layers)
  }

  def medianReduce(inField: ScalarFieldMemory): Float = {
    // unpaddedArray is a copy of the data, as required by array-modifying
    // nthOrderedElement routine.
    val retValue =
      if (EnableApproximateMedian && inField.fieldShape.points > ApproxMedianEnableThreshold) {
        val samples = inDim match {
          case 3 => Array.tabulate(ApproxMedianEnableThreshold) {
            i => inField.read(layerIndex(i), rowIndex(i), columnIndex(i))
          }
          case 2 => Array.tabulate(ApproxMedianEnableThreshold) {
            i => inField.read(rowIndex(i), columnIndex(i))
          }
          case 1 => Array.tabulate(ApproxMedianEnableThreshold) {
            i => inField.read(columnIndex(i))
          }
          case 0 => Array.tabulate(ApproxMedianEnableThreshold) {
            i => inField.read() // shouldn't get to here frankly
          }
        }
        median(samples)
      }
      else {
        // Pull full data array out and calculate exact median
        val unpaddedArray = inField.readAsUnpaddedArray
        median(unpaddedArray)
      }
    retValue
  }
  
  private def median(array: Array[Float]): Float = {
    val midpoint = array.length/2        // = the higher-indexed of two middle elements for an even length array
    val median = nthOrderedElement(array, midpoint)
    if (array.length % 2 == 1) {
      // Odd number of elements in the array, 'median' is the middle element
      median
    } else if (array(midpoint-1) == median) {
      // Even number of elements in the array, so our median so far is
      // really the higher-indexed of two medians that we want to
      // take the average of.  While the array is not perfectly sorted
      // at this point, we've opportunistically checked to see if there's
      // an identical value sitting in the array(midpoint-1) spot.
      median
    }
    else {
      // Still handling the even number of array elements case.  Here,
      // the element that is the next smaller one compared to our upper median
      // is the max value in the [0,midpoint-1] region.
      (median + maxOverWindow(array, 0, midpoint-1))/2.0f
    }
  }

  // return the maximum value over a window of the array
  private def maxOverWindow(array: Array[Float], lowerBound: Int, upperBound: Int): Float = {
    var maxVal = array(lowerBound)    
    for (i <- lowerBound+1 to upperBound) {
      if (array(i) > maxVal)
        maxVal = array(i)
    }
    maxVal
  }
  
/* This routine maintains a window into the array that is known
 * to contain the nth-smallest element (this window starts out
 * as the whole array).  On each pass of the algorithm, a candidate
 * for the n-th smallest element is selected and the array is
 * sorted by means of element swapping, but only with respect to
 * this n-th element value.  From this sorting, it is known how
 * many elements are greater or less than this n-th element candidate.
 * Further, it can be inferred whether the n-th element candidate
 * was too big or too small, and based on this, the n-th element
 * containing window is shrunk and a new n-th candidate is selected. When
 * the window is reduced to a single array element (necessarily at position
 * n), the nth element in a small->large ordering has been determined.
 */
  private def nthOrderedElement(array: Array[Float], n: Int): Float = {
    var nthWindowLowerBound = 0
    var nthWindowUpperBound = array.length - 1
    while (nthWindowLowerBound < nthWindowUpperBound) {
      val nthCandidate = array(n)
      var lowerSwapIndex = nthWindowLowerBound
      var upperSwapIndex = nthWindowUpperBound
      do {
        // Starting from the low end of the nth-element-containing
        // window, find an element greater or equal to the nthCandidate
        while (array(lowerSwapIndex) < nthCandidate)
          lowerSwapIndex += 1
        // Starting from the high end of the nth-element-containing
        // window, find an element smaller or equal to the nthCandidate
        while (nthCandidate < array(upperSwapIndex))
          upperSwapIndex -= 1
        // As long as the swap indexes have not crossed,
        // swap the two elements, and bump the indexes
        if (lowerSwapIndex <= upperSwapIndex) {
          // swap i-th and j-th elements
          val temp = array(lowerSwapIndex)
          array(lowerSwapIndex) = array(upperSwapIndex)
          array(upperSwapIndex) = temp
          lowerSwapIndex += 1
          upperSwapIndex -= 1
        }
      } while (lowerSwapIndex <= upperSwapIndex)

      // Opportunistic widening of the swapIndex positions, makes algorithm
      // more efficient where many of the same values are present.
      while (lowerSwapIndex < array.length && array(lowerSwapIndex) == nthCandidate)
        lowerSwapIndex += 1

      while (upperSwapIndex >= 0 && array(upperSwapIndex) == nthCandidate)
        upperSwapIndex -= 1

      // At the point where the lower- and upperSwapIndexes cross,
      // because of the swapping we've done, we now know that all
      // elements lower in the array are smaller or equal to the nthCandidate,
      // and all elements higher in the array are greater or equal.  If this
      // swap index cross-over point is at index n, we've found the
      // nth smallest.  Otherwise, we know whether to pick a larger
      // or smaller next nthCandidate, and we can remove a chunk
      // of the array from our nth-smallest-containing window.
      if (upperSwapIndex < n) {
        // All the elements at the upperSwapIndex and above are
        // greater than the nthCandidate, but we've determined here
        // there are too many. Thus, our nthCandidate is too small.
        // Since all the elements lower in the array than the lowerSwapIndex
        // are less than the nthCandidate, we discard that lower region
        // from further consideration.
        nthWindowLowerBound = lowerSwapIndex
      }
      if (n < lowerSwapIndex) {
        // All the elements at the lowerSwapIndex and below are
        // smaller than the nthCandidate, but we've determined here
        // there are too many. Thus, our nthCandidate is too big.
        // Since all the elements higher in the array than the upperSwapIndex
        // are greater than the nthCandidate, we discard that upper region
        // from further consideration.
        nthWindowUpperBound = upperSwapIndex
      }
    }
    return array(n)
  }

  def compute(in: Array[OpenCLFieldRegister], out: OpenCLFieldRegister) {
    val inputFieldMemory = in(0).slave.read.asInstanceOf[ScalarFieldMemory]
    val result = medianReduce(inputFieldMemory)
    val outputFieldMemory = out.master.cpuMemory.asInstanceOf[ScalarFieldMemory]
    outputFieldMemory.write(result)
    // Copy the CPU memory to the GPU.
    out.master.write
  }

  /** Create a clone of this kernel that uses a new set of virtual field registers
    *  as inputs.  Useful for breaking a large circuit apart into smaller subcircuits. */
  def copyWithNewInputs(inputs: Array[VirtualFieldRegister]): AbstractKernel = {
    require(inputs.length == 1)
    new CPUScalarReduceMedianKernel(inputs(0), opcode, resultType)
  }
}

private[cogx]
object CPUScalarReduceMedianKernel {

  def apply(in: VirtualFieldRegister, operation: Opcode, resultType: FieldType) = {
    require(in.fieldType.tensorOrder == 0)
    val expectedResultType = new FieldType(Shape(), Shape(), Float32)
    require(resultType == expectedResultType)
    require(operation == FieldReduceMedianOp)

    new CPUScalarReduceMedianKernel(in, operation, resultType)
  }
}
