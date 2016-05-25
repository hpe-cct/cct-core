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

package cogx.compiler.gpu_operator.function

import cogx.platform.types._
import cogx.platform.types.ElementTypes._
import cogx.compiler.parser.semantics.SemanticError
import scala.collection.mutable.ArrayBuffer
import cogx.compiler.parser.syntaxtree.Field
import cogx.compiler.CompilerError
import cogx.compiler.gpu_operator.statement.{EndAnonymous, AnonymousBlock, Assignment}
import cogx.compiler.gpu_operator.declaration.{ConstantExpression, GPUArrayVariable, NamedVariable, GPUVariable}
import cogx.compiler.gpu_operator.Constraints
import cogx.compiler.gpu_operator.types.GPUType
import cogx.compiler.gpu_operator.expression.{ReadTensorElementExpression, ReadTensorExpression, Operator, GPUExpression}

/** Glue between CPU and GPU. Buffer that holds a tensor field. Logically this
  * is a single buffer, but current GPU's tend to have a CPU version and a GPU
  * version, with copying between them.
  *
  * Color images require special handling because they are handled in an
  * unexpected way on GPUs. Although color image pixels are typically stored
  * as four unsigned bytes packed into a 32-bit word, on the GPU they look
  * like float4. We special case this here.
  *
  * @param field The tensor field.
  * @param index The index of the input tensor field, starting from 0.
  */
private[gpu_operator]
class FieldBuffer private(val field: Field, index: Int)
        extends SemanticError
        with Constraints
        with CompilerError
{
  /** Unique ID for the buffer. */
  val fieldType: FieldType = field.fieldType

  /** Type of the tensor held in the field buffer. This is lazily evaluated
    * because fields holding big tensors don't have a legitimate tensor type
    * (OpenCL and CUDA don't support them). This means you can't execute
    * small tensor operators (such as tensorVar and readTensor) on a big
    * tensor field.
    */
  lazy val tensorType: GPUType = {
    fieldType.elementType match {
      case Uint8Pixel =>
        // We only handle color images for now. Note that 8-bit pixels get
        // translated to floats on GPU
        GPUType(Float32, 4)
      case Float32 =>
        fieldType.tensorShape.points match {
          case 1 => GPUType(Float32, 1)
          case 2 => GPUType(Float32, 2)
          case 3 => GPUType(Float32, 3)
          case 4 => GPUType(Float32, 4)
          case x =>
            check(required = false, "tensors too big, max elements = " + MaxTensorSize)
            throw new Exception("fatal error")
        }
      case Complex32 =>
        fieldType.tensorShape.points match {
          case 1 => GPUType(Float32, 2)
          case x =>
            check(required = false, "only complex scalar fields supported")
            throw new Exception("fatal error")
        }
      case x =>
        throw new Exception("unsupported element type: " + x)
    }
  }

  /** Type of the element held in tensors. Hardcoded since generic fields
    * are not yet supported.
    */
  val elementType: GPUType =
    fieldType.elementType match {
      case Uint8Pixel =>
        GPUType(Float32, 1)
      case Float32 =>
        GPUType(Float32, 1)
      case Complex32 =>
        GPUType(Float32, 2)
      case x =>
        throw new Exception("unsupported element type: " + x)
    }

  /** Read a tensor from the field buffer and assign it to a variable.
    *
    * This uses implicit (_layer, _row, _column) addressing. Note that if
    * the field is 0D, though, it reads the single tensor in the field.
    *
    * @return Variable holding the tensor that was read.
    */
  def readTensor(): GPUVariable = {
    val variable = GPUVariable(tensorType)
    val value = ReadTensorExpression(fieldType, tensorType, index, None)
    Assignment(variable, "=", value)
    variable
  }

  /** Read a tensor from a 1D field buffer and assign it to a variable.
    *
    * @param indices The address of the desired tensor in the field.
    * @return Variable holding the tensor that was read.
    */
  def readTensor(indices: Array[GPUExpression]): GPUVariable = {
    val variable = GPUVariable(tensorType)
    // Hyperkernels require hardcoded names for indexing, namely "layer",
    // "row" and "column". We declare these using named variables. However,
    // since they may have already been declared previously, we declare them
    // within an anonymous block to avoid name clashes.
    AnonymousBlock()
    val value = ReadTensorExpression(fieldType, tensorType, index, Some(indices))
    Assignment(variable, "=", value)
    EndAnonymous()
    variable
  }

  /** Is the user's tensor element index the special local index designator "_tensorElement"? */
  private def isLocalElement(element: GPUExpression) = {
    element == ConstantExpression._tensorElement
  }

  //The next two methods require "tensorElement" for addressing, so we need to create
  //an anonymous block to generate the appropriate code.

  /** Read a tensor element from the field buffer and assign it to a variable.
    *
    * This uses implicit (_layer, _row, _column) addressing. Note that if
    * the field is 0D, though, it reads the single tensor in the field.
    *
    * @param element The index of the element in the tensor to read.
    * @return Variable holding the tensor that was read.
    */
  def readTensorElement(element: GPUExpression): GPUVariable = {
    val variable = GPUVariable(elementType)
    if (!element.gpuType.isInt)
      error("integer expression required for tensor element index")
    // The readElement macro in HyperKernels requires a parameter named
    // "tensorElement." Since that may have already been defined, we create
    // an anonymous block to prevent conflicts.
    AnonymousBlock()
    val tensorElement = NamedVariable(GPUType(Int32, 1), "tensorElement")
    tensorElement := element
    val value =
      ReadTensorElementExpression(fieldType, elementType, index, None, isLocalElement(element))
    Assignment(variable, "=", value)
    EndAnonymous()
    variable
  }


  /** Read a tensor element from a 1D field buffer and assign it to a variable.
    *
    * @param indices The address of the desired tensor in the field.
    * @param element The index of the element in the tensor to read.
    * @return Variable holding the tensor that was read.
    */
  def readTensorElement(indices: Array[GPUExpression],
                        element: GPUExpression): GPUVariable =
  {
    if (!element.gpuType.isInt)
      error("integer expression required for tensor element index")
    val variable = GPUVariable(elementType)
    // Hyperkernels require hardcoded names for indexing, namely "layer",
    // "row" and "column". We declare these using named variables. However,
    // since they may have already been declared previously, we declare them
    // within an anonymous block to avoid name clashes.
    // The same also applies to the tensor element since the readElement
    // macro in HyperKernel requires a parameter named "tensorElement."
    AnonymousBlock()
    // Create "tensorElement" local variable
    val tensorElement = NamedVariable(GPUType(Int32, 1), "tensorElement")
    tensorElement := element
    val value =
      ReadTensorElementExpression(fieldType, elementType, index, Some(indices), isLocalElement(element))
    Assignment(variable, "=", value)
    EndAnonymous()
    variable
  }

  /** Declare a variable that has the same type as tensors in the field.
    */
  def tensorVar(): GPUVariable = {
    GPUVariable(tensorType)
  }

  /** Declare an n-D array that has the same type as tensors in the field.
    *
    * @param size The dimensionality of the field.
    */
  def tensorArray(size: Array[GPUExpression]): GPUArrayVariable = {
    if (size.length > MaxArrayDimensions)
      error("Too many dimensions for array declaration, max = " +
        MaxArrayDimensions)
    new GPUArrayVariable(tensorType, size)
  }

  /** Declare a variable that has the same type as tensor elements in the field.
    */
  def tensorElementVar(): GPUVariable = {
    new GPUVariable(elementType)
  }

  /** Declare an n-D array that has the same type as tensor elements in
    * the field.
    *
    * @param size The dimensionality of the field.
    */
  def tensorElementArray(size: Array[GPUExpression]): GPUArrayVariable = {
    if (size.length > MaxArrayDimensions)
      error("Too many dimensions for array declaration, max = " +
        MaxArrayDimensions)
    new GPUArrayVariable(elementType, size)
  }
}


/** Factory for creating field buffers.
  *
  * This is designed to parse individual GPU functions while
  * maintaining separate field buffers for each. When a GPU function has
  * been parsed, the `getFieldBuffers` method should be called which
  * returns all field buffers that have been allocated for that function.
  * This call has the side effect of flushing all field buffers that have
  * been created for the current GPU function, thus initializing the world
  * for parsing the next GPU function.
  *
  * This object formerly had the field:
  *
  * private var buffers = new ArrayBuffer[FieldBuffer]
  *
  * We now use a thread-local version to enable simultaneous Cog compilation from multiple threads.
  */
private[gpu_operator]
object FieldBuffer {
  /** All statements in a function. Each thread gets its own instance starting with a zero-length ArrayBuffer. */
  private val _buffers = new ThreadLocal[ArrayBuffer[FieldBuffer]] {
    override def initialValue() = new ArrayBuffer[FieldBuffer]
  }
  private def buffers = _buffers.get()
  private def buffers_=(newBufferss: ArrayBuffer[FieldBuffer]) { _buffers.set(newBufferss) }

  /** Create a FieldBuffer for a field. If a buffer for that field already
    * exists, it is reused.
    *
    * @param field The field for which a field buffer will be allocated or
    *        reused
    * @return The field buffer for field.
    */
  def create(field: Field): FieldBuffer = {
    // See if a buffer already exists for the field.
    for (buffer <- buffers)
      if (buffer.field eq field)
        return buffer
    // No, need to create a new one.
    val id = buffers.length
    val fieldBuffer = new FieldBuffer(field, id)
    buffers += fieldBuffer
    fieldBuffer
  }

  /** Get all field buffers that have been allocated. Calling this empties
    * the internal cache of field buffers.
    */
  def getFieldBuffers: Array[FieldBuffer] = {
    val bufferArray = buffers.toArray
    buffers = new ArrayBuffer[FieldBuffer]
    bufferArray
  }
}
