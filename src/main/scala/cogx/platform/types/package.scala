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

package cogx.platform

/** Implements the core computational abstractions in Cog:
  *
  * 1. Fields, which are multidimensional arrays of data. The FieldType class
  * describes the structure of the field (shape), while the ElementType class
  * describes the elements within that field (e.g. floating point numbers,
  * pixels, ...)
  *
  * 2. Buffers, memory regions on the CPU and GPU which hold Fields.
  *
  * 3. Kernels, which take fields as inputs and compute resulting fields as
  * outputs. The AbstractKernel class is the base representation for all
  * kernels, and extends the Node class so that kernels may be connected
  * together into directed acyclic graphs, called Circuits. The Circuit
  * structure supplies the dependence structure of the computation, so that
  * the kernels may be properly scheduled for execution. Each kernel is assigned
  * an Opcode which designates the computation that it performs.
  *
  * @author Greg Snider
  */
package object types

