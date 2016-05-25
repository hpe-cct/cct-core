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

package cogx.helper

import cogx.compiler.parser.syntaxtree.ColorField
import cogx.cogmath.geometry.Shape
import cogx.reference._
import cogx.platform.types.Pixel

/**
 * Glue code that adds cog3 style initialization of new ScalarFields using RefScalarFields.
 *
 */
trait ColorFieldBuilderInterface {

  /** Create a "rows" x "columns" real scalar field. */
  def TestColorField(rows: Int, columns: Int) : ColorField =
    TestColorField(Shape(rows, columns))

  /** Create a ColorField with shape "shape". */
  def TestColorField(shape: Shape): ColorField =
    TestColorField(shape, RefColorField(shape))

  /** Create a ColorField initialized by "initialState". */
  def TestColorField(initialState: => RefColorField): ColorField = {
    // Call initial state function once, rather than allow it to be called once for each red(), green() and blue()
    // method invocation in the last routine below.
    val initState: RefColorField = initialState
    TestColorField(initState.fieldShape, initState)
  }

  /** Create a ColorField with shape "shape" and state "initialState".*/
  def TestColorField(shape: Shape, initialState: => RefColorField) : ColorField = {
    val red = initialState.red
    val green = initialState.green
    val blue = initialState.blue

    def pixel(r: Int, c: Int) = {
      new Pixel(red.read(r,c), green.read(r,c), blue.read(r,c))
    }

    shape.dimensions match {
      case 2 => ColorField(shape(0), shape(1), (x, y) => pixel(x, y))
      case x => throw new RuntimeException("illegal number of dimensions"); null
    }
  }
}
