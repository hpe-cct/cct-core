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

package cogx

/** The raw GPU platform on which Cog runs. This basically supplies single
  * GPU support for a computation--multiple GPUs, perhaps distributed across
  * a network, must be supported by a higher level package.
  *
  * In principle it can support both OpenCL and CUDA, and could be expanded to
  * support other platforms as well.
  *
  * @author Greg Snider
  */
package object platform

