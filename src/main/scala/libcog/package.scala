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

import cogx._

/** The Cog library.
  *
  * @author Greg Snider
  */
package object libcog extends CogXInterface {
  //----------------------------------------------------------------------------
  //Simplified functions for scala math on floats
  //----------------------------------------------------------------------------
  type FloatMath = floatmath.FloatMath

  //----------------------------------------------------------------------------
  //Non-core tensor and field types
  //----------------------------------------------------------------------------
  type StickTensor = fields.StickTensor
  val StickTensor = fields.StickTensor
  type StickTensorField = fields.StickTensorField
  val StickTensorField = fields.StickTensorField

  type SymmetricTensor = fields.SymmetricTensor
  val SymmetricTensor = fields.SymmetricTensor
  type SymmetricTensorField = fields.SymmetricTensorField
  val SymmetricTensorField = fields.SymmetricTensorField
}
