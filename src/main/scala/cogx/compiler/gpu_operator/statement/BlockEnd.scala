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

import cogx.compiler.gpu_operator.expression.GPUExpression

/** A statement ending a block.
 *
 * @author Greg Snider
 */
private[gpu_operator]
sealed abstract class BlockEnd extends Statement {
  override def toString: String = "}"
}

/** End of `if`. */
private[gpu_operator]
case class EndIf() extends BlockEnd {
  constructed()
}

/** End of `else`. */
private[gpu_operator]
case class EndElse() extends BlockEnd {
  constructed()
}

/** End of `else if`. */
private[gpu_operator]
case class EndElseIf() extends BlockEnd {
  constructed()
}

/** End of `while`. */
private[gpu_operator]
case class EndWhile() extends BlockEnd {
  constructed()
}

/** End of `for`. */
private[gpu_operator]
case class EndFor() extends BlockEnd {
  constructed()
}

/** End of `forEachTensorElement`. */
private[gpu_operator]
case class EndForEachTensorElement() extends BlockEnd {
  constructed()
}

/** End of anonymous block. */
private[gpu_operator]
case class EndAnonymous() extends BlockEnd {
  constructed()
}

