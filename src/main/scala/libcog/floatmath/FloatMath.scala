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

package libcog.floatmath

/** Floating point versions of Scala math functions missing from scala.math
  *
  * @author Greg Snider
  */
trait FloatMath {
  def acos(x: Float) = math.acos(x).toFloat
  def asin(x: Float) = math.asin(x).toFloat
  def atan(x: Float) = math.atan(x).toFloat
  def atan2(x: Float, y: Float) = math.atan2(x, y).toFloat
  def cbrt(x: Float) = math.cbrt(x).toFloat
  def ceil(x: Float) = math.ceil(x).toFloat
  def cos(x: Float) = math.cos(x).toFloat
  def cosh(x: Float) = math.cosh(x).toFloat
  def exp(x: Float) = math.exp(x).toFloat
  def expm1(x: Float) = math.expm1(x).toFloat
  def floor(x: Float) = math.floor(x).toFloat
  def hypot(x: Float, y: Float) = math.hypot(x, y).toFloat
  def log(x: Float) = math.log(x).toFloat
  def log10(x: Float) = math.log10(x).toFloat
  def log1p(x: Float) = math.log1p(x).toFloat
  def pow(x: Float, y: Float) = math.pow(x, y).toFloat
  def sin(x: Float) = math.sin(x).toFloat
  def sinh(x: Float) = math.sinh(x).toFloat
  def sq(x:Float) = x*x
  def sqrt(x: Float) = math.sqrt(x).toFloat
  def tan(x: Float) = math.tan(x).toFloat
  def tanh(x: Float) = math.tanh(x).toFloat
  def toDegrees(x: Float) = math.toDegrees(x).toFloat
  def toRadians(x: Float) = math.toRadians(x).toFloat

  val Pi = math.Pi.toFloat
}