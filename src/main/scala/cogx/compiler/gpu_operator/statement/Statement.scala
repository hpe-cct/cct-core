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

import cogx.compiler.gpu_operator.declaration.GPUVariable

import scala.collection.mutable.ArrayBuffer
import cogx.parameters.Cog

/** A statement in a program. This consists of
  *
  * 1. Variable declarations.
  *
  * 2. Block declarations
  *
  * 3. End block declarations.
  *
  * 4. Function declarations.
  *
  * @author Greg Snider
  */
private[gpu_operator]
trait Statement {
  Statement.register(this)

  /** Called by subclasses to signal that construction is complete. This
    * is used for debugging.
    */
  protected def constructed() {
    if (Cog.verboseUserGPUOperators)
      println("  statement  >    " + this.toString)
  }
}

/** Companion object for Statement.
  *
  * This maintains an array of all statements within a function. When the
  * function definition is complete, `getStatements()` should be called
  * to recover the statements and get the object ready to start
  * accumulating statements for the next function.
  *
  * This object formerly had the field:
  *
  * private var statements = new ArrayBuffer[Statement]
  *
  * We now use a thread-local version to enable simultaneous Cog compilation from multiple threads.
  */
private[gpu_operator]
object Statement {
  /** All statements in a function. Each thread gets its own instance starting with a zero-length ArrayBuffer. */
  private var _statements =  new ThreadLocal[ArrayBuffer[Statement]] {
    override def initialValue() = new ArrayBuffer[Statement]
  }
  private def statements = _statements.get()
  private def statements_=(newStatements: ArrayBuffer[Statement]) { _statements.set(newStatements) }

  /** Register a statement for the current function. */
  private def register(statement: Statement) {
    statements += statement
  }

  /** Unregister a statement for the current function. */
  def unregister(statement: Statement): Unit = {
    statements -= statement
  }

  /** Get all registered statements. */
  def getStatements(): Array[Statement] = {
    val statementArray = statements.toArray
    statements = new ArrayBuffer[Statement]
    GPUVariable.reset()
    statementArray
  }
}