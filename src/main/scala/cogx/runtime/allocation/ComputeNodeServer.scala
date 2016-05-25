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

/** An autonomous process running on a compute node that has two
  * responsibilities:
  *
  * 1. Replies to queries about GPU resources, returning a list of all GPUs and
  * their characteristics to the caller.
  *
  * 2. Creates and destroys actors on demand on behalf of the caller.
  *
  * This process is launched by the ClusterSupervisor which then issues the
  * queries and requests to it listed above.
  *
  * @author Greg Snider
  */
private[runtime]
class ComputeNodeServer {

}