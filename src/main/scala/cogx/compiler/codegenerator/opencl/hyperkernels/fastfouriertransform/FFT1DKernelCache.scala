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

package cogx.compiler.codegenerator.opencl.hyperkernels.fastfouriertransform

import scala.collection.mutable.Map
import cogx.cogmath.algebra.real.Logarithm._

/** A cache of OpenCL kernels that implement the 1D FFT.
  *
  * @author Greg Snider
  */
private[cogx]
object FFT1DKernelCache {
  /** Cache of FFT planners, indexed by FFT size and OpenCl Device. */
  private val plannerCache = Map[(Int, Int), ClFFTPlanner]()

  /**
    * Get the number of passes (kernels) needed to implement a 1D "columns" FFT on "device".
    */
  def kernelNames(columns: Int, deviceMaxWorkItemsPerWorkGroup: Int): Array[String] = {
    val planner = lookupPlanner(columns, deviceMaxWorkItemsPerWorkGroup)
    planner.kernelNames
  }

  /**
    * Get the source code for a 1D FFT for a 1D "columns" FFT executed on OpenCL "device".
    */
  def sourceCodes(columns: Int, deviceMaxWorkItemsPerWorkGroup: Int): Array[String] = {
    val planner = lookupPlanner(columns, deviceMaxWorkItemsPerWorkGroup)
    planner.clSourceCode
  }

  /**
    * Get the work dimensions for a 1D FFT "columns" FFT executed on OpenCL "device".
    */
  def workDimensions(columns: Int, deviceMaxWorkItemsPerWorkGroup: Int): Array[WorkDimensions] =
  {
    val planner = lookupPlanner(columns, deviceMaxWorkItemsPerWorkGroup)
    planner.workDimensions
  }

  /**
    * Get an FFT Planner for a 1D "columns" FFT for "device".
    */
  private def lookupPlanner(columns: Int, deviceMaxWorkItemsPerWorkGroup: Int): ClFFTPlanner = {
    require(columns > 0 && isPowerOf2(columns))
    synchronized {
      val tuple = (columns, deviceMaxWorkItemsPerWorkGroup)
      val dataFormat = SplitComplexFormat // We no longer support Interleaved

      if (!(plannerCache contains tuple))
        plannerCache(tuple) = new ClFFTPlanner(deviceMaxWorkItemsPerWorkGroup, columns, dataFormat)
      plannerCache(tuple)
    }
  }
}