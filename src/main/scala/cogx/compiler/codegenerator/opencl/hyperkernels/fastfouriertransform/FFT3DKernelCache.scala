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

import cogx.cogmath.algebra.real.Logarithm._

/** A cache of OpenCL kernels that implement the 3D FFT. The 3D FFT requires
  * three kernels, Pass0 Pass1 and Pass2, which must be streamed together:
  * {{{
  *    inputBuffer --> Pass0 --> interBuffer --> Pass1 --> interBuffer --> Pass2 --> outputBuffer
  * }}}
  *
  * @author Greg Snider
  */
private[cogx]
object FFT3DKernelCache {
  /**Cache of FFT planners, indexed by FFT size and OpenCl Device. */
  private val plannerCache = collection.mutable.Map[(Int, Int, Int, Int), ClFFTPlanner]()

  /**
    * Get the number of passes (kernels) needed to implement a "rows" x
    * "columns x layers" FFT on "device".
    */
  def kernelNames(layers: Int, rows: Int, columns: Int, deviceMaxWorkItemsPerWorkGroup: Int): Array[String] = {
    val planner = lookupPlanner(layers, rows, columns, deviceMaxWorkItemsPerWorkGroup)
    planner.kernelNames
  }

  /**
    * Get the source code for a 3D FFT for a "rows" x "columns" x "layers" FFT
    * executed on OpenCL "device".
    */
  def sourceCodes(layers: Int, rows: Int, columns: Int, deviceMaxWorkItemsPerWorkGroup: Int): Array[String] = {
    val planner = lookupPlanner(layers, rows, columns, deviceMaxWorkItemsPerWorkGroup)
    planner.clSourceCode
  }

  /**
    * Get the work dimensions for a 3D FFT "pass" for a "rows" x "columns" x "layers" FFT
    * executed on OpenCL "device".
    */
  def workDimensions(layers: Int, rows: Int, columns: Int, deviceMaxWorkItemsPerWorkGroup: Int): Array[WorkDimensions] = {
    val planner = lookupPlanner(layers, rows, columns, deviceMaxWorkItemsPerWorkGroup)
    planner.workDimensions
  }

  /**
    * Get an FFT Planner for a "rows" x "columns" x "layers" FFT for "device".
    */
  private def lookupPlanner(layers: Int, rows: Int, columns: Int, deviceMaxWorkItemsPerWorkGroup: Int): ClFFTPlanner = {
    require(layers > 0 && rows > 0 && columns > 0 && isPowerOf2(layers) && isPowerOf2(rows) && isPowerOf2(columns))
    synchronized {
      val tuple = (layers, rows, columns, deviceMaxWorkItemsPerWorkGroup)
      val dataFormat = SplitComplexFormat // We no longer support Interleaved

      if (!(plannerCache contains tuple))
        plannerCache(tuple) = new ClFFTPlanner(deviceMaxWorkItemsPerWorkGroup, layers, rows, columns, dataFormat)
      plannerCache(tuple)
    }
  }
}
