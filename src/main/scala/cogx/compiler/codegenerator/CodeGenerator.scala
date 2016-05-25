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

package cogx.compiler.codegenerator

import cogx.compiler.parser.syntaxtree.SyntaxTree
import cogx.platform.types.AbstractKernel

/** Interface for a code generator, translating a syntax tree to a kernel
  * circuit.
  *
  * @author Greg Snider
  */
private[cogx]
trait CodeGenerator[T <: AbstractKernel] {

  /** Map a SyntaxTree to a KernelDAG. */
  def generateCircuit(syntaxTree: SyntaxTree): KernelCircuit

  /** Remove dangling pointers to resources, allowing the garbage collector
    * to dispose of them after they have been deallocated on the platform.
    *
    * @param syntaxTree Syntax tree to be de-annotated with the results of
    *        generateCircuit.
    */
  def releaseCircuit(syntaxTree: SyntaxTree): Unit
}