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

import cogx.compiler.parser.semantics.SemanticError

/** Represents operators that execute within a CUDA / OpenCL kernel.
  *
  * @author Greg Snider
  */
private[gpu_operator]
class Operator(val name: String)
        extends SemanticError
{
  override def toString = name

  /** Compare two operators for equality. */
  override def equals(that: Any): Boolean = {
    that match {
      case other: Operator => this.name equals other.name
      case _ => false
    }
  }

  /** Override required by equals override. */
  override def hashCode: Int = name.hashCode
}
