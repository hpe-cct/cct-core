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

package cogx.platform.types

import java.util.NoSuchElementException

/** Facility for saving/restoring a Cog kernel code type.
  *
  * The 'stock' Scala sealed abstract class lacks a factory maker of class instances given a stored id.
  * The apply() method below achieves this, but requires that all additions or deletions to the case objects
  * be reflected in the 'codeTypes' Seq.  Important then to keep this in-sync!!!
  *
  * A plain Scala enumeration lacks the compile-time exhaustive match checking of sealed abstract classes.
  * I was also concerned that over time there might be deletions in the list of supported code types.  It
  * would be important to keep the id's the same for the values that had already been stored (i.e. better
  * to allow holes to appear in the sequence of valid id's than to shift the id assignments).
  *
  * Created by Dick Carter.
  */

object KernelCodeTypes {
  /** The kernel code language. */
  sealed abstract class KernelCodeType(val name: String) {
    override def toString = name
  }

  /** The OpenCL kernel code language. */
  case object CogOpenCL extends KernelCodeType("CogOpenCL")

  /* The NVIDIA CUDA kernel code language. */
  case object CogCUDA   extends KernelCodeType("CogCUDA")

  /* No language, as might be useful in unit tests. */
  case object CogNone   extends KernelCodeType("CogNone")

  /** The universe of CogKernelCodeTypes: Important to keep this consistent with the above!!! */
  private val codeTypes = Seq(CogOpenCL, CogCUDA, CogNone)

  private val codeTypesString = codeTypes.mkString(" or ")

  private val idToKernelCodeType = codeTypes.map(x => (x.name, x)).toMap

  /** Factory method for creating the CogKernelCodeType from its stored id */
  def apply(name: String) = try {
    idToKernelCodeType(name)
  } catch {
    case e: NoSuchElementException =>
      throw new RuntimeException(s"Illegal CogKernelCodeType $name, expecting $codeTypesString")
  }
}

