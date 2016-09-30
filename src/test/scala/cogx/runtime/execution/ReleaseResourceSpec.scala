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

package cogx.runtime.execution

/*
 * HP Confidential
 * © Copyright 2008-2013 Hewlett-Packard Development Company, L.P.
 *
 * Confidential computer software. Valid license from HP required for
 * possession, use or copying.  Consistent with FAR 12.211 and 12.212,
 * Commercial Computer Software, Computer Software Documentation, and
 * Technical Data for Commercial Items are licensed to the U.S. Government
 * under vendor's standard commercial license.
 *
 *                      DISCLAIMER OF WARRANTY
 *
 * The following software: Cog ex Machina is experimental and is provided as
 * a courtesy, free of charge, "AS-IS" by Hewlett-Packard Development Company,
 * L.P. ("HP"). HP shall have no obligation to maintain or support this
 * software. HP MAKES NO EXPRESS OR IMPLIED WARRANTY OF ANY KIND REGARDING
 * THIS SOFTWARE. HP SHALL NOT BE LIABLE FOR ANY DIRECT, INDIRECT, SPECIAL,
 * INCIDENTAL OR CONSEQUENTIAL DAMAGES, WHETHER BASED ON CONTRACT, TORT OR
 * ANY OTHER LEGAL THEORY, IN CONNECTION WITH OR ARISING OUT OF THE
 * FURNISHING, PERFORMANCE OR USE OF THIS SOFTWARE.
 */

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import org.junit.runner.RunWith
import cogx.api.{CogFunctionAPI, ImplicitConversions}
import cogx.compiler.parser.syntaxtree.{UnpipelinedActuator, UnpipelinedSensor}
import cogx.platform.opencl.OpenCLPlatform
import cogx.runtime.ComputeGraph
import cogx.runtime.allocation.AllocationMode

/** Test code for memory freeing of GPU global and CPU non-heap memory.
  *
  * @author Greg Snider and Dick Carter
  */

@RunWith(classOf[JUnitRunner])
class ReleaseResourceSpec
        extends FunSuite
        with MustMatchers
        with ImplicitConversions
        with CogFunctionAPI
{
  /** Create a number of ComputeGraphs in series, releasing the resources
    * between ComputeGraphs.  Do this enough times to generate an allocation
    * failure if the resources are not properly released.
    *
    * The parameter `sizeBytes` dictates how much memory is allocated for each
    * ComputeGraph, both in GPU global memory and in CPU non-heap memory
    * (a.k.a. direct memory).
    */
  test("release of memory resources") {

    val numBufsPerComputeGraph = 2 // constant input buffer and output buffer

    def createAndReleaseComputeGraph(deviceIndex: Int, sizeBytes: Long) {
      val elements = (sizeBytes / 4 / numBufsPerComputeGraph).toInt

      val cg = new ComputeGraph(device = Some(deviceIndex)) {
        val inArray = new Array[Float](elements)
        val outArray = new Array[Float](elements)
        val input = new UnpipelinedSensor(elements, () => inArray)
        val output = input + 1.0f
        UnpipelinedActuator(output, outArray)
        probe(output, "")
      }
      try {
        cg.step
      }
      finally
        cg.release
    }

    // To keep the runtime of this test down, we assume at least a 500MB
    // global memory GPU, of which we can grab 400MB.  If GPU global memory
    // were not freed, this test would still pass on a system with a GPU
    // having more than 12GB of global memory.  Also, if CPU non-heap memory
    // ("direct" memory) were not being freed, this test would still pass
    // on a system with around 64GB of physical memory, for which the default
    // allocation for direct memory would be around 16GB.
    val desiredAllocSize = 400L * 1024 * 1024       // 400MB

    val deviceIndex = AllocationMode.default match {
      case AllocationMode.SingleGPU(device) => device
      case _ => 0
    }

    def sizeToMB(size: Long) = s"${size/1024/1024} MB"

    val device = OpenCLPlatform().devices(deviceIndex)
    val deviceGlobalMemory = device.globalMemSize
    val shouldntFitSize = (1.1 * deviceGlobalMemory).toLong
    val bufAllocSize = math.min(desiredAllocSize, device.maxMemAllocSize)
    if (bufAllocSize < desiredAllocSize)
      println(s"Warning: downsizing to buffer alloc size ${sizeToMB(bufAllocSize)}, rather than the test's default of ${sizeToMB(desiredAllocSize)}.")
    val cgAllocSize = numBufsPerComputeGraph * bufAllocSize

    val numComputeGraphs = (shouldntFitSize + cgAllocSize - 1) / cgAllocSize
    println(s"Creating $numComputeGraphs ComputeGraphs each of size ${sizeToMB(cgAllocSize)} to exceed the device global memory of ${sizeToMB(deviceGlobalMemory)} if they aren't released.")

    for (i <- 0L until numComputeGraphs)
      createAndReleaseComputeGraph(deviceIndex, cgAllocSize)
  }
}
