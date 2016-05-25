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

package cogx.compiler.codegenerator.opencl.fragments

/** The type of writing that a hyperkernel performs.
  *
  * @author Dick Carter
  */

private[cogx]
sealed abstract class WriteType(val isNonLocal: Boolean)

/** Initialization, before write type known. */
private[cogx]
case object WriteUnknown extends WriteType(false)

/** Kernel does not write at end of execution. */
private[cogx]
case object WriteNull extends WriteType(false)

/** Write, index based on _row, _column, _layer, and possibly _tensorElement */
private[cogx]
case object WriteLocal extends WriteType(false)

/** Write, index based on row, column, layer, and possibly tensorElement */
private[cogx]
case object WriteNonlocal extends WriteType(true)

/** Write, index based on row, column, layer, and tensorElement */
private[cogx]
case object WriteElementNonlocal extends WriteType(true)


