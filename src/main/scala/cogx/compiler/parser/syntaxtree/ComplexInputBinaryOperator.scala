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

package cogx.compiler.parser.syntaxtree

import cogx.compiler.parser.op._

/** Factory for creating complex binary operators. This is a simple wrapper
  * for BinaryOperator that ensures the input fields have been converted
  * to ComplexFields if needed.
  *
  * @author Dick Carter
  */
private[cogx]
object ComplexInputBinaryOperator {

  /** Create a built-in binary operator operating on two fields.  Since the
    * inputs are typed as ComplexFields, the implicit conversion will be
    * invoked to convert them via a RealToComplexOp as necessary.
    *
    * @param operation The operation performed on the two fields.
    * @param in1 First input field.
    * @param in2 Second input field.
    *
    * @author Greg Snider
    */
  def apply(operation: BinaryOpcode, in1: Field, in2: Field): Field =
    BinaryOperator(operation, CogFunctions.toGenericComplexField(in1),
      CogFunctions.toGenericComplexField(in2))
}