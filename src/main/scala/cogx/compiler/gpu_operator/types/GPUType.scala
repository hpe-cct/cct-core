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

package cogx.compiler.gpu_operator.types

import cogx.platform.types.ElementTypes._

/** Fundamental types on a GPU, as represented by CUDA and OPENCL. Since
  * both programming models support vectors, we treat all types as though
  * they are vectorized. Scalars are represented as vectors of length 1.
  *
  * @param elementType Type of element held in the vector type.
  * @param elements Number of elements in the vector.
  *
  * @author Greg Snider
  */
private[gpu_operator]
case class GPUType(elementType: ElementType, elements: Int = 1) {

  /** Allowable number of elements in a GPU vector type. */
  val allowableElements = Array(1, 2, 3, 4)
  require(allowableElements contains elements,
    "illegal number of elements in array: " + elements)

  /** Return the appropriate zero for this type as a string. */
  def zero: String = {
    "(" + toString + ")(0)"
  }

  /** Convert the type to a string understood by OpenCL or CUDA. */
  override def toString: String = {
    val buffer = new StringBuffer
    buffer append elementType.cTypeName
    if (elements != 1)
      buffer append elements.toString
    buffer.toString
  }

  /** Test two types for C equivalence.
    *
    * @param other Type to compare with `this`.
    * @return True if the two types are equivalent
    */
  override def equals(other: Any): Boolean = {
    other match {
      case GPUType(elType, size) =>
        (elementType == elType) && (elements == size)
      case _ =>
        false
    }
  }

  /** Override needed because of equals override. */
  override def hashCode: Int = {
    elementType.hashCode + elements
  }

  /** True if type is a floating point type. */
  val isFloat: Boolean = {
    elementType match {
      case Float16 => true
      case Float32 => true
      case Float64 => true
      case _ => false
    }
  }

  /** True if type is an integral type. */
  val isInt: Boolean = {
    elementType match {
      case Int8 => true
      case Uint8 => true
      case Int16 => true
      case Uint16 => true
      case Int32 => true
      case Uint32 => true
      case Int64 => true
      case Uint64 => true
      case _ => false
    }
  }

  /** True if type is a signed integral type. */
  val isSignedInt: Boolean = {
    elementType match {
      case Int8 => true
      case Int16 => true
      case Int32 => true
      case Int64 => true
      case _ => false
    }
  }

  /** True if type is an unsigned integral type. */
  val isUnsignedInt: Boolean = {
    elementType match {
      case Uint8 => true
      case Uint16 => true
      case Uint32 => true
      case Uint64 => true
      case _ => false
    }
  }

  /** Number of bits needed to represent the type. */
  val bits: Int = {
    elementType match {
      case Int8 => 8 * elements
      case Uint8 => 8 * elements
      case Int16 => 16 * elements
      case Uint16 => 16 * elements
      case Int32 => 32 * elements
      case Uint32 => 32 * elements
      case Int64 => 64 * elements
      case Uint64 => 64 * elements
      case Float16 => 16 * elements
      case Float32 => 32 * elements
      case Float64 => 64 * elements
      case Complex16 => 2 * 16 * elements
      case Complex32 => 2 * 32 * elements
      case Complex64 => 2 * 64 * elements
      case Uint8Pixel => 8 * elements
    }
  }

  /** Return true if vector has only one element. */
  def isScalar: Boolean =
    elements == 1

  /** Converts a signed or unsigned integer type to its unsigned version. */
  def toUnsigned: GPUType = {
    elementType match {
      case Int8 =>   GPUType(Uint8, elements)
      case Uint8 =>  GPUType(Uint8, elements)
      case Int16 =>  GPUType(Uint16, elements)
      case Uint16 => GPUType(Uint16, elements)
      case Int32 =>  GPUType(Uint32, elements)
      case Uint32 => GPUType(Uint32, elements)
      case Int64 =>  GPUType(Uint64, elements)
      case Uint64 => GPUType(Uint64, elements)
      case _ =>
        throw new Exception("internal error, attempt to convert non-integral type to unsigned")
    }
  }

  /** Converts a signed or unsigned integer type to its unsigned version. */
  def toSigned: GPUType = {
    elementType match {
      case Int8 =>   GPUType(Uint8, elements)
      case Uint8 =>  GPUType(Uint8, elements)
      case Int16 =>  GPUType(Uint16, elements)
      case Uint16 => GPUType(Uint16, elements)
      case Int32 =>  GPUType(Uint32, elements)
      case Uint32 => GPUType(Uint32, elements)
      case Int64 =>  GPUType(Uint64, elements)
      case Uint64 => GPUType(Uint64, elements)
      case _ =>
        throw new Exception("internal error, attempt to convert non-integral type to unsigned")
    }
  }

  /** True if type is a complex type. */
  val isComplex: Boolean = {
    elementType match {
      case Complex16 => true
      case Complex32 => true
      case Complex64 => true
      case _ => false
    }
  }

  /** True if type is a pixel type. */
  val isPixel: Boolean = {
    elementType match {
      case Uint8Pixel => true
      case _ => false
    }
  }
}
