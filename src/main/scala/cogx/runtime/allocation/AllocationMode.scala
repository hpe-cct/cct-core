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

package cogx.runtime.allocation

/** A specification of the machines and devices that will be allocated to run the ComputeGraph.
  *
  * @author Dick Carter
  */
sealed trait AllocationMode
object AllocationMode {
  /** Mode for single machine, single device. */
  case class SingleGPU(deviceIndex: Int) extends AllocationMode
  /** Mode for single machine, all devices. */
  case object MultiGPU extends AllocationMode
  /** Mode for multiple machines, all their devices. */
  case object Cluster extends AllocationMode
  /** Determine the allocation mode if none was specified explicitly: uses JVM System Property cog.device */
  def default: AllocationMode = {
    Option(System.getProperty("cog.device")) match {
      case Some("all") =>
        Console.err.println("[AllocateCluster] Warning: Operating in experimental multi-GPU mode.")
        AllocationMode.MultiGPU
      case _ =>
        AllocationMode.SingleGPU(System.getProperty("cog.device","0").toInt) // Default mode
    }
  }
}
