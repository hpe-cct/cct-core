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

private[cogx]
object ElementTypes {

  /** Types of elements in tensors in fields.  Some types listed here
    * are placeholders for future support.
    *
    * @param name Name of the type as stored in the file-representation of ComputeGraphs.
    * @param cTypeName Name of the type in C (and OpenCL and CUDA).
    */
  @SerialVersionUID(6916364269088189221L)
  sealed abstract class ElementType(val name: String, val cTypeName: String) extends Serializable {
    override def toString = name
  }

  /** 16 bit float */
  case object Float16 extends ElementType("Float16", "half")

  /** 32 bit float */
  case object Float32 extends ElementType("Float32", "float")

  /** 64 bit float */
  case object Float64 extends ElementType("Float64", "double")

  /** 8 bit signed */
  case object Int8 extends ElementType("Int8", "char")

  /** 16 bit signed */
  case object Int16 extends ElementType("Int16", "short")

  /** 32 bit signed */
  case object Int32 extends ElementType("Int32", "int")

  /** 64 bit signed */
  case object Int64 extends ElementType("Int64", "long")

  /** 8 bit unsigned */
  case object Uint8 extends ElementType("Uint8", "unsigned char")

  /** 16 bit unsigned */
  case object Uint16 extends ElementType("Uint16", "unsigned short")

  /** 32 bit unsigned */
  case object Uint32 extends ElementType("Uint32", "unsigned int")

  /** 64 bit unsigned */
  case object Uint64 extends ElementType("Uint64", "unsigned long")

  /** 8 bit pixel, seen as float on GPU. */
  case object Uint8Pixel extends ElementType("Uint8Pixel", "float")

  /** two 16-bit float */
  case object Complex16 extends ElementType("Complex16", "half2")

  /** two 32-bit float */
  case object Complex32 extends ElementType("Complex32", "float2")

  /** two 64-bit float */
  case object Complex64 extends ElementType("Complex64", "double2")

  /** The universe of CogKernelCodeTypes: Important to keep this consistent with the above!!! */
  private val elementTypes = Seq(Float16, Float32, Float64,
                                 Int8, Int16, Int32, Int64,
                                 Uint8, Uint16, Uint32, Uint64,
                                 Uint8Pixel,
                                 Complex16, Complex32, Complex64)

  private val elementTypesString = elementTypes.mkString(",  ")

  private val nameToElementType = elementTypes.map(x => (x.name, x)).toMap

  /** Factory method for creating the ElementType from its stored name */
  def apply(nameAsStored: String) = try {
    nameToElementType(nameAsStored)
  } catch {
    case e: NoSuchElementException =>
      throw new RuntimeException(s"Illegal CogKernelCodeType id $nameAsStored, expecting one of $elementTypesString.")
  }
}