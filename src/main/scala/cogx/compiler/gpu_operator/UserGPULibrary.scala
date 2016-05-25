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

package cogx.compiler.gpu_operator

import cogx.cogmath.geometry.Shape
import cogx.platform.types.ElementTypes.Float32
import cogx.platform.types.FieldType

import scala.language.implicitConversions
import cogx.compiler.gpu_operator.function._
import cogx.compiler.gpu_operator.declaration.{ArrayLValue, VariableDeclarations}
import cogx.compiler.gpu_operator.statement._
import cogx.compiler.gpu_operator.expression.{ArrayElementPointerExpression, VariablePointerExpression}

/** Supplies access to functions needed to create user-defined GPU operators.
  *
  * @author Greg snider
  */
trait UserGPULibrary
        extends AtomicFunctions
        with BlockFunctions
        with CommonFunctions
        with ConstantExpressionFunctions
        with FieldReadFunctions
        with FieldVariableFunctions
        with FieldWriteFunctions
        with GeometricFunctions
        with IntegerFunctions
        with MathFunctions
        with RelationalFunctions
        with TypeConversionFunctions
        with VariableDeclarations
        with VectorLiteralFunctions
{
  /** A scalar/vector variable in a GPU kernel */
  type GPUVariable = cogx.compiler.gpu_operator.declaration.GPUVariable

  /** An array variable in a GPU kernel */
  type GPUArrayVariable = cogx.compiler.gpu_operator.declaration.GPUArrayVariable

  /** An expression in a GPU kernel */
  type GPUExpression = cogx.compiler.gpu_operator.expression.GPUExpression

  // Output field indices
  val _out0 = cogx.compiler.gpu_operator.types._out0
  val _out1 = cogx.compiler.gpu_operator.types._out1
  val _out2 = cogx.compiler.gpu_operator.types._out2
  val _out3 = cogx.compiler.gpu_operator.types._out3
  val _out4 = cogx.compiler.gpu_operator.types._out4
  val _out5 = cogx.compiler.gpu_operator.types._out5
  val _out6 = cogx.compiler.gpu_operator.types._out6
  val _out7 = cogx.compiler.gpu_operator.types._out7
  val _out8 = cogx.compiler.gpu_operator.types._out8
  val _out9 = cogx.compiler.gpu_operator.types._out9

  /** Implicitly convert an int to an expression. */
  implicit def intToConstExpression(value: Int): GPUExpression = {
    ConstInt0D(value)
  }

  /** Implicitly convert a float to an expression. */
  implicit def floatToConstExpression(value: Float): GPUExpression = {
    ConstFloat0D(value)
  }

  /** Synchronize threads with a barrier with respect to local memory. */
  def _syncThreadsLocal = SyncThreadsLocalStatement()

  /** Synchronize threads with a barrier with respect to global memory. */
  def _syncThreadsGlobal = SyncThreadsGlobalStatement()

  /** Debug the GPUOperator. */
  def _debug = DebugStatement()


  /** Override local thread allocation (work group size).
    *
    * @param shape The shape of the work group threads.
    * @return Statement for the local thread allocation override
    */
  def _localThreads(shape: Shape) = LocalThreadsStatement(shape)

  /** Override global thread allocation.
    *
    * @param fieldType The field type on which to model thread allocation; the
    *                  thread allocator will treat this as though the output
    *                  tensor field were of this type, which typically means
    *                  you'll get a thread per tensor (or tensor element in
    *                  some cases)
    * @return Statement for the global thread allocation override.
    */
  @deprecated("use _globalThreads(Shape) or _globalThreads(Shape, Shape) instead",
    "Cog 4.1")
  def _globalThreads(fieldType: FieldType) = GlobalThreadsStatement(fieldType)

  /** Override global thread allocation. Typically used to create a thread per
    * tensor in a field, though the field may be illusory, not corresponding
    * to any input or output field. This gives you raw control over thread
    * allocation.
    *
    * Each thread can see its identity in the global thread space from
    * the constants:
    * {{{
    *   _layer
    *   _row
    *   _column
    * }}}
    *
    * @param fieldShape The shape of the global threads allocated. This may be
    *                   1D, 2D or 3D, and each thread can see its identity
    *                   with the _layer, _row, and _column constants.
    * @return Statement for the global thread allocation override.
    */
  def _globalThreads(fieldShape: Shape) =
    GlobalThreadsStatement(new FieldType(fieldShape, Shape(), Float32))

  /** Override global thread allocation. Typically used to create a thread per
    * tensor element in a field, though the field may be illusory, not
    * corresponding to any input or output field. This gives you raw control
    * over thread allocation.
    *
    * Each thread can see its identity in the global thread space from
    * the constants:
    * {{{
    *   _layer
    *   _row
    *   _column
    *   _tensorElement
    * }}}
    * Basically this creates a thread for each element in a tensor field.
    *
    * @param fieldShape The shape of the global threads allocated. This may be
    *                   1D, 2D or 3D, and each thread can see its identity
    *                   with the _layer, _row, and _column constants.
    * @param tensorShape The shape of the tensors in the (possibly illusory)
    *                    field. One thread is created for each tensor in that
    *                    field.
    * @return Statement for the global thread allocation override.
    */
  def _globalThreads(fieldShape: Shape, tensorShape: Shape) =
    GlobalThreadsStatement(new FieldType(fieldShape, tensorShape, Float32))

  /** Declare a variable to be local. */
  def _local(variable: GPUVariable): GPUVariable = {
    variable.isShared = true
    variable
  }

  /** Declare an array variable to be local. */
  def _local(variable: GPUArrayVariable): GPUArrayVariable = {
    variable.isShared = true
    variable
  }

  /** Declare a variable to be volatile. */
  def _volatile(variable: GPUVariable): GPUVariable = {
    variable.isVolatile = true
    variable
  }

  /** Declare an array variable to be volatile. */
  def _volatile(variable: GPUArrayVariable): GPUArrayVariable = {
    variable.isVolatile = true
    variable
  }

  /** Create a pointer to a variable. */
  def _pointerTo(variable: GPUVariable) =
    VariablePointerExpression(variable)

  /** Create a pointer to an array element. */
  def _pointerTo(arrayElement: ArrayLValue) =
    ArrayElementPointerExpression(arrayElement)
}


