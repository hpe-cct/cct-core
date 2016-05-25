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

package cogx.compiler.cpu_operator

import java.lang.reflect.Method
import cogx.platform.types.FieldType
import cogx.compiler.parser.semantics.SemanticError
import cogx.platform.cpumemory._

/** Object which executes a user-operator.
  *
  * User-operators run on the CPU, so this class combined with the Operator
  * class provide the glue necessary for user-operators to interact with fields
  * located on the GPU.
  *
  * @param name The name of the operator as defined by the user.
  * @param operator The operator object, needed for method invocation.
  * @param computeMethod The method corresponding to the user's `compute`
  *        method, which must be executed to implement the operator's
  *        execution.
  * @param fieldType The type of field produced by the operator.
  * @param result The field writer which the user code uses to: (1) define
  *        the type of result for a given set of input fields; and
  *        (2) provides the FieldWriter which the user compute method writes
  *        its output to.
  *
  * @author Greg Snider
  */
private[cogx]
class OperatorExecutor(val name: String,
                       operator: Object,
                       computeMethod: Method,
                       val fieldType: FieldType,
                       result: FieldWriterProxy)
        extends SemanticError
{
  /** Array for holding the arguments passed to the user's compute method. */
  private val arguments =
    new Array[Object](computeMethod.getParameterTypes.length)

  /** Execute the operator.
    *
    * @param in Sequence of fields serving as inputs to the operator.
    * @param out Field where result is to be written by the user's compute
    *        method.
    */
  private[cogx] def execute(in: AbstractFieldMemory*)(out: AbstractFieldMemory) {
    // Concatenate inputs and output into arguments.
    var index = 0
    for (input <- in.toArray) {
      arguments(index) = input
      index += 1
    }
    arguments(index) = out
    result match {
      case proxy: ScalarFieldWriterProxy =>
        proxy.setWriter(out.asInstanceOf[ScalarFieldMemory])
      case proxy: VectorFieldWriterProxy =>
        proxy.setWriter(out.asInstanceOf[VectorFieldMemory])
      case proxy: MatrixFieldWriterProxy =>
        proxy.setWriter(out.asInstanceOf[MatrixFieldMemory])
      case proxy: ColorFieldWriterProxy =>
        proxy.setWriter(out.asInstanceOf[ColorFieldMemory])
      case proxy: ComplexFieldWriterProxy =>
        proxy.setWriter(out.asInstanceOf[ComplexFieldMemory])
      case x =>
        throw new RuntimeException("unknown field type")
    }
    // Pass exceptions up to next level
    try {
      computeMethod.invoke(operator, arguments :_*)
    }
    catch {
      case e: Exception =>
        throw new RuntimeException("User CPU Operator '" + name + "' throws an exception in its compute() method!", e)
    }
  }

  /** Meaningful name to appear in the KernelCircuit, as output by Cog.printOptimized=true */
  override def toString: String = name
}
