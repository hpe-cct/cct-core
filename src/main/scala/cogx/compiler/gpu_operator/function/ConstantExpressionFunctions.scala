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

import cogx.compiler.gpu_operator.FloatConstantsTrait
import cogx.compiler.gpu_operator.expression.{Operator, GPUExpression, NulleryIntExpression}
import cogx.compiler.gpu_operator.types.GPUType
import cogx.platform.types.ElementTypes.{Float32, Int32}

/** Functions for creating user constant expressions and Cog thread-local
  * constants.
  *
  * @author Greg Snider
  */
trait ConstantExpressionFunctions extends FloatConstantsTrait {
  // Cog thread-local constants
  val _layer = new NulleryIntExpression(" _layer ")
  val _row = new NulleryIntExpression(" _row ")
  val _column = new NulleryIntExpression(" _column ")

  val _layers = new NulleryIntExpression(" _layers ")
  val _rows = new NulleryIntExpression(" _rows ")
  val _columns = new NulleryIntExpression(" _columns ")

  val _localLayers = new NulleryIntExpression(" _localLayers ")
  val _localRows = new NulleryIntExpression(" _localRows ")
  val _localColumns = new NulleryIntExpression(" _localColumns ")

  val _localLayer = new NulleryIntExpression(" _localLayer ")
  val _localRow = new NulleryIntExpression(" _localRow ")
  val _localColumn = new NulleryIntExpression(" _localColumn ")

  val _tensorElement = new NulleryIntExpression(" _tensorElement ")
  val _tensorElements = new NulleryIntExpression(" _tensorElements ")


  // The following are defined ONLY if local memory has been allocated
  val _groupLayer = new NulleryIntExpression(" _groupLayer ")
  val _groupRow = new NulleryIntExpression(" _groupRow ")
  val _groupColumn = new NulleryIntExpression(" _groupColumn ")

  /** An Int constant as a GPUExpression. */
  case class ConstInt0D(value: Int)
          extends GPUExpression(new Operator(""), GPUType(Int32), Array())
  {
    override def toString: String = value.toInt.toString
    override def exprString = toString
  }

  /** A Float constant as a GPUExpression. */
  case class ConstFloat0D(value: Float)
          extends GPUExpression(new Operator(""), GPUType(Float32), Array())
  {
    // Hardwired to OpenCL representation.  This must change with multiple backends.
    override def toString = toOpenCLString(value)
    override def exprString = toString
  }
}