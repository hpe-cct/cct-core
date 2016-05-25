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

package cogx.cogmath.algebra.real

/** Functions for combining two arrays of Floats.
  *
  * In all of these functions,
  * one of the two arguments may be null, in which case the function returns
  * the non-null argument. If both arguments are null, the function returns null.
  * If both arguments are non-null, both arguments must be of the same length.
  *
  * User: Greg Snider
  * Date: Nov 12, 2010
  * Time: 8:03:11 AM
  */

trait ArrayMath {

  /** Compute "a" + "b".
    *
    * One of the two arguments may be null, in which case the function returns
    * the non-null argument. If both arguments are null, the function returns
    * null. If both arguments are non-null, both arguments must be of the same
    * length.
    */
  def add(a: Array[Float], b: Array[Float]): Array[Float] = {
    if (b == null)
      a
    else if (a == null)
      b
    else {
      require(a.length == b.length)
      val result = new Array[Float](a.length)
      Array.copy(a, 0, result, 0, a.length)
      for (i <- 0 until result.length)
        result(i) += b(i)
      result
    }
  }

  /** Compute "a" - "b".
    *
    * One of the two arguments may be null, in which case the function returns
    * the non-null argument. If both arguments are null, the function returns
    * null. If both arguments are non-null, both arguments must be of the same
    * length.
    */
  def subtract(a: Array[Float], b: Array[Float]): Array[Float] = {
    if (b == null)
      a
    else if (a == null)
      b
    else {
      require(a.length == b.length)
      val result = new Array[Float](a.length)
      Array.copy(a, 0, result, 0, a.length)
      for (i <- 0 until result.length)
        result(i) -= b(i)
      result
    }
  }

  /** Compute "a" * "b".
    *
    * One of the two arguments may be null, in which case the function returns
    * the non-null argument. If both arguments are null, the function returns
    * null. If both arguments are non-null, both arguments must be of the same
    * length.
    */
  def multiply(a: Array[Float], b: Array[Float]): Array[Float] = {
    if (b == null)
      a
    else if (a == null)
      b
    else {
      require(a.length == b.length)
      val result = new Array[Float](a.length)
      Array.copy(a, 0, result, 0, a.length)
      for (i <- 0 until result.length)
        result(i) *= b(i)
      result
    }
  }

  /** Compute "a" / "b".
    *
    * One of the two arguments may be null, in which case the function returns
    * the non-null argument. If both arguments are null, the function returns
    * null. If both arguments are non-null, both arguments must be of the same
    * length.
    */
  def divide(a: Array[Float], b: Array[Float]): Array[Float] = {
    if (b == null)
      a
    else if (a == null)
      b
    else {
      require(a.length == b.length)
      val result = new Array[Float](a.length)
      Array.copy(a, 0, result, 0, a.length)
      for (i <- 0 until result.length)
        result(i) /= b(i)
      result
    }
  }

  /** Compute "a" + "b".  If "a" is null, returns null.
   */
  def add(a: Array[Float], b: Float): Array[Float] = {
    if (a == null)
      null
    else {
      val result = new Array[Float](a.length)
      Array.copy(a, 0, result, 0, a.length)
      for (i <- 0 until result.length)
        result(i) += b
      result
    }
  }

  /** Compute "a" - "b".  If "a" is null, returns null.
   */
  def subtract(a: Array[Float], b: Float): Array[Float] = {
    if (a == null)
      null
    else {
      val result = new Array[Float](a.length)
      Array.copy(a, 0, result, 0, a.length)
      for (i <- 0 until result.length)
        result(i) -= b
      result
    }
  }

  /** Compute "a" * "b".  If "a" is null, returns null.
   */
  def multiply(a: Array[Float], b: Float): Array[Float] = {
    if (a == null)
      null
    else {
      val result = new Array[Float](a.length)
      Array.copy(a, 0, result, 0, a.length)
      for (i <- 0 until result.length)
        result(i) *= b
      result
    }
  }

  /** Compute "a" / "b".  If "a" is null, returns null.
   */
  def divide(a: Array[Float], b: Float): Array[Float] = {
    if (a == null)
      null
    else {
      val result = new Array[Float](a.length)
      Array.copy(a, 0, result, 0, a.length)
      for (i <- 0 until result.length)
        result(i) /= b
      result
    }
  }
}
