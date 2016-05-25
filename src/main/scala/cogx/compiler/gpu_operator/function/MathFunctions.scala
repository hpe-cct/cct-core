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

import cogx.compiler.parser.semantics.SemanticError
import cogx.compiler.gpu_operator.expression._


/** Built-in math functions.
  *
  * @author Greg Snider
  */
trait MathFunctions extends SemanticError {
  def _acos(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("acos"), expr)

  def _acosh(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("acosh"), expr)

  def _acospi(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("acospi"), expr)

  def _asin(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("asin"), expr)

  def _asinh(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("asinh"), expr)

  def _asinpi(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("asinpi"), expr)

  def _atan(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("atan"), expr)

  def _atan2(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryFloatExpression(new Operator("atan2"), expr1, expr2)

  def _atanh(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("atanh"), expr)

  def _cbrt(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("cbrt"), expr)

  def _ceil(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("ceil"), expr)

  def _copysign(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryFloatExpression(new Operator("copysign"), expr1, expr2)

  def _cos(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("cos"), expr)

  def _cosh(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("cosh"), expr)

  def _cospi(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("cospi"), expr)

  def _native_divide(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryFloatExpression(new Operator("native_divide"), expr1, expr2)

  def _erfc(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("erfc"), expr)

  def _erf(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("erf"), expr)

  def _exp(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("exp"), expr)

  def _exp2(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("exp2"), expr)

  def _exp10(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("exp10"), expr)

  def _expm1(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("expm1"), expr)

  def _fabs(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("fabs"), expr)

  def _fdim(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryFloatExpression(new Operator("fdim"), expr1, expr2)

  def _floor(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("floor"), expr)

  def _fma(expr1: GPUExpression, expr2: GPUExpression, expr3: GPUExpression) =
    new TernaryFloatExpression(new Operator("fma"), expr1, expr2, expr3)

  def _fmax(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryFloatExpression(new Operator("fmax"), expr1, expr2)

  def _fmin(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryFloatExpression(new Operator("fmin"), expr1, expr2)

  def _fmod(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryFloatExpression(new Operator("fmod"), expr1, expr2)

  def _hypot(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryFloatExpression(new Operator("hypot"), expr1, expr2)

  def _lgamma(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("lgamma"), expr)

  def _log(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("log"), expr)

  def _log2(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("log2"), expr)

  def _log10(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("log10"), expr)

  def _log1p(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("log1p"), expr)

  def _logb(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("logb"), expr)

  def _mad(expr1: GPUExpression, expr2: GPUExpression, expr3: GPUExpression) =
    new TernaryFloatExpression(new Operator("mad"), expr1, expr2, expr3)

  def _maxmag(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryFloatExpression(new Operator("maxmag"), expr1, expr2)

  def _minmag(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryFloatExpression(new Operator("minmag"), expr1, expr2)

  def _nextafter(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("nextafter"), expr)

  def _pow(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryFloatExpression(new Operator("pow"), expr1, expr2)

  def _pown(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryFloatIntExpression(new Operator("pown"), expr1, expr2)

  def _powr(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryFloatExpression(new Operator("powr"), expr1, expr2)

  def _remainder(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryFloatExpression(new Operator("remainder"), expr1, expr2)

  def _rint(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("rint"), expr)

  def _round(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("round"), expr)

  def _rsqrt(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("rsqrt"), expr)

  def _sin(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("sin"), expr)

  def _sinh(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("sinh"), expr)

  def _sinpi(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("sinpi"), expr)

  def _sqrt(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("sqrt"), expr)

  def _tan(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("tan"), expr)

  def _tanh(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("tanh"), expr)

  def _tanpi(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("tanpi"), expr)

  def _tgamma(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("tgamma"), expr)

  def _trunc(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("trunc"), expr)
}

