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

package cogx.compiler.gpu_operator.function

import cogx.compiler.gpu_operator.expression._

/** Built-in atomic functions.
  *
  * @author Greg snider
  */
trait AtomicFunctions {

  def _atomic_add(pointer: PointerExpression, value: GPUExpression) =
    new BinaryAtomicExpression(new Operator("atomic_add"), pointer, value)

  def _atomic_sub(pointer: PointerExpression, value: GPUExpression) =
    new BinaryAtomicExpression(new Operator("atomic_sub"), pointer, value)

  def _atomic_xchg(pointer: PointerExpression, value: GPUExpression) =
    new BinaryAtomicExpression(new Operator("atomic_xchg"), pointer, value)

  def _atomic_inc(pointer: PointerExpression) =
    new UnaryAtomicExpression(new Operator("atomic_inc"), pointer)

  def _atomic_dec(pointer: PointerExpression) =
    new UnaryAtomicExpression(new Operator("atomic_dec"), pointer)

  def _atomic_cmpxchg(pointer: PointerExpression,
                      value1: GPUExpression,
                      value2: GPUExpression) =
    new TernaryAtomicExpression(new Operator("atomic_cmpxchg"), pointer, value1, value2)

  def _atomic_min(pointer: PointerExpression, value: GPUExpression) =
    new BinaryAtomicExpression(new Operator("atomic_min"), pointer, value)

  def _atomic_max(pointer: PointerExpression, value: GPUExpression) =
    new BinaryAtomicExpression(new Operator("atomic_max"), pointer, value)

  def _atomic_and(pointer: PointerExpression, value: GPUExpression) =
    new BinaryAtomicExpression(new Operator("atomic_and"), pointer, value)

  def _atomic_or(pointer: PointerExpression, value: GPUExpression) =
    new BinaryAtomicExpression(new Operator("atomic_or"), pointer, value)

  def _atomic_xor(pointer: PointerExpression, value: GPUExpression) =
    new BinaryAtomicExpression(new Operator("atomic_xor"), pointer, value)
}
