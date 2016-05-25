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

import cogx.compiler.parser.syntaxtree.ScalarField
import cogx.cogmath.geometry.Shape
import cogx.reference._
import cogx.cogmath.algebra.real.{Vector, Matrix}

/**
 * Glue code that adds cog3 style initialization of new ScalarFields using RefScalarFields.
 *
 */
trait ScalarFieldBuilderInterface {

/** Create a 0-D real scalar field. */
  def TestScalarField(): ScalarField =
    TestScalarField(Shape(), RefScalarField(0f))

  /** Create a "columns" length real scalar field. */
  def TestScalarField(columns: Int): ScalarField =
    TestScalarField(Shape(columns), RefScalarField(columns))

  /** Create a "rows" x "columns" real scalar field. */
  def TestScalarField(rows: Int, columns: Int) : ScalarField =
    TestScalarField(Shape(rows, columns), RefScalarField(rows, columns))

  /** Create a "layers" x "rows" x "columns" real scalar field. */
  def TestScalarField(layers: Int, rows: Int, columns: Int) : ScalarField=
    TestScalarField(Shape(layers, rows, columns), RefScalarField(layers, rows, columns))

  /** Create a real scalar field with shape "shape". */
  def TestScalarField(shape: Shape): ScalarField =
    TestScalarField(shape, RefScalarField(shape))

  /** Create a ScalarField initialized by "initialState". */
  def TestScalarField(initialState: => RefScalarField): ScalarField = {
    // Call initial state function once, rather than allow it to be called once for each read()
    // method invocation in the last routine below.
    val initState: RefScalarField = initialState
    TestScalarField(initState.fieldShape, initState)
  }

  /** Create a ScalarField initialized by "matrix". */
  def TestScalarField(matrix: Matrix) =
    ScalarField(matrix.rows, matrix.columns, (r,c) => matrix(r,c))

  /** Create a ScalarField initialized by "vector". */
  def TestScalarField(vector: Vector) =
    ScalarField(vector.length, (c) => vector(c))

  /** Create a ScalarField with shape "shape" and state "initialState".*/
  def TestScalarField(shape: Shape, initialState: => RefScalarField) : ScalarField = {
    shape.dimensions match {
      case 0 => ScalarField(initialState.read())
      case 1 => ScalarField(shape(0), (x) => initialState.read(x))
      case 2 => ScalarField(shape(0), shape(1), (x, y) => initialState.read(x, y))
      case 3 => ScalarField(shape(0), shape(1), shape(2), (x, y, z) => initialState.read(x, y, z))
      case x => throw new RuntimeException("illegal number of dimensions"); null
    }
  }
}
