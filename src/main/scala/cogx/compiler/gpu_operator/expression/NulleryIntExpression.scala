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

package cogx.compiler.gpu_operator.expression

import cogx.compiler.gpu_operator.types.GPUType
import cogx.platform.types.ElementTypes.Int32

/** Expression which takes no arguments. This is used for built-in field
  * constants.
  *
  * @param name The name of the built-in field constant.
  */
private[gpu_operator]
class NulleryIntExpression(val name: String)
        extends GPUExpression(new Operator("nullery"), GPUType(Int32), Array())
{
  override def exprString = name
  override def toString = name

  override def equals(other: Any): Boolean = other match {
    case exp: NulleryIntExpression => this.name == exp.name
    case _ => false
  }

  override def hashCode = name.hashCode
}