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

package cogx.compiler.codegenerator.opencl.hyperkernels.discretecosinetransform

import cogx.cogmath.algebra.real.Logarithm._
import cogx.compiler.codegenerator.opencl.hyperkernels.fastfouriertransform.{SplitComplexFormat, WorkDimensions}

import scala.collection.mutable.Map

/** A cache of OpenCL kernels that implement the 2D FFT. The 2D FFT requires
  * two kernels, Pass0 and Pass1, which must be streamed together:
  * {{{
  *    inputBuffer --> Pass0 --> interBuffer --> Pass1 --> outputBuffer
  * }}}
  *
  * @author Greg Snider
  */
private[cogx]
object DCT2DKernelCache {
  /** Cache of DCTT planners, indexed by FFT size and OpenCl Device. */
  private val plannerCache = Map[(Int, Int, Boolean), DCTPlanner]()
  private val DeviceMaxWorkItemsPerWorkGroup = 256

  /**
   * Get the number of passes (kernels) needed to implement a "rows" x
   * "columns" FFT on "device".
   */
  def kernelName(rows: Int, columns: Int, forward: Boolean): String = {
    val planner = lookupPlanner(rows, columns, forward)
    planner.kernelNames
  }

  /**
   * Get the source code for a 2D FFT for a "rows" x "columns" FFT
   * executed on OpenCL "device".
   */
  def sourceCode(rows: Int, columns: Int, forward: Boolean): String = {
    val planner = lookupPlanner(rows, columns, forward)
    planner.clSourceCode
  }

  /**
   * Get the work dimensions for a 2D FFT "pass" for a "rows" x "columns" FFT
   * executed on OpenCL "device".
   */
  def workDimensions(rows: Int, columns: Int, forward: Boolean): WorkDimensions =
  {
    val planner = lookupPlanner(rows, columns, forward)
    planner.workDimensions
  }

  /**
   * Get an FFT Planner for a "rows" x "columns" FFT for "device".
   */
  private def lookupPlanner(rows: Int, columns: Int, forward: Boolean): DCTPlanner =
  {
    require(rows > 0 && columns > 0 && isPowerOf2(rows) && isPowerOf2(columns))
    synchronized {
      val tuple = (rows, columns, forward)
      val dataFormat = SplitComplexFormat  // we no longer support Interleaved

      if (!(plannerCache contains tuple))
        plannerCache(tuple) = new DCTPlanner(DeviceMaxWorkItemsPerWorkGroup,
          rows, columns, forward)
      plannerCache(tuple)
    }
  }
}