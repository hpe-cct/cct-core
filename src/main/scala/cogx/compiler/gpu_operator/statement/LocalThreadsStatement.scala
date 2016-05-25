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

package cogx.compiler.gpu_operator.statement

import cogx.cogmath.geometry.Shape
import cogx.compiler.parser.semantics.SemanticError

/** Statement to override the work group size.
  *
  * @param workGroupShape The shape of the work group threads.
  */
case class LocalThreadsStatement(workGroupShape: Shape)
  extends Statement
  with SemanticError
{
  if (workGroupShape.dimensions != 2)
    error("_localThreads shape must be 2 dimensional")
  constructed()

  /** This statement does not generate executable code. */
  override def toString =
    "// Overriding local threads: " + workGroupShape.toString
}
