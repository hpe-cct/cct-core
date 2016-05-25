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

package cogx.compiler.parser.semantics

import cogx.platform.types.FieldType
import cogx.compiler.codegenerator.common.FieldPolicies._


/** Computes the default result type when a standard binary operator is applied
  * to two inputs. Some operators may override this if they can handle
  *
  * @author Greg Snider and Dick Carter
  */
private[cogx]
object ResultType {

  /** Deduce the default output type given two input types.
    *
    * The rules:
    *
    * 1. If they both have the same field type (dimensions and tensor order),
    * the result has the same type.
    *
    * 2. If they both have the same field shape, but at least one of the fields
    * is a scalar field, the result has the type of the larger-tensor-order field.
    *
    * 3. If they both have the same tensor shape, but at least one of the fields
    * is 0-dimensional, the result has the type of the larger field.
    *
    * 4. If one of the fields is a 0-dimensional scalar field, the result is
    * the type of the other field.
    *
    * @param _in1 Type of the first input.
    * @param _in2 Type of the second input.
    * @return Option of result type.
    */
  def apply(_in1: FieldType, _in2: FieldType): Option[FieldType] = {

    // Generally, result will be complex if either input is.
    val in1: FieldType = if (isComplexField(_in2)) toComplex(_in1) else _in1
    val in2: FieldType = if (isComplexField(_in1)) toComplex(_in2) else _in2

    if (in1 == in2)
      Some(in1)
    else if (in1.tensorShape == in2.tensorShape && is0DField(in1))
      Some(in2)
    else if (in1.tensorShape == in2.tensorShape && is0DField(in2))
      Some(in1)
    else if (in1.fieldShape == in2.fieldShape && isTensor0Field(in1))
      Some(in2)
    else if (in1.fieldShape == in2.fieldShape && isTensor0Field(in2))
      Some(in1)
    else if (is0DField(in1) && isTensor0Field(in1))
      Some(in2)
    else if (is0DField(in2) && isTensor0Field(in2))
      Some(in1)
    else
      None

  }
}