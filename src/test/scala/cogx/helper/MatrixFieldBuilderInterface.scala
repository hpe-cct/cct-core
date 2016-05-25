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

import cogx.compiler.parser.syntaxtree.MatrixField
import cogx.cogmath.geometry.Shape
import cogx.reference._
import cogx.cogmath.algebra.real.Matrix

/**
 * Glue code that adds cog3 style initialization of new MatrixFields using
 * RefMatrixFields.
 *
 */
trait MatrixFieldBuilderInterface {


  /** Create a zero-dimensional real matrix field. */
  def TestMatrixField(matrixShape: Shape) : MatrixField  =
    TestMatrixField(Shape(), matrixShape, RefMatrixField(matrixShape))

  /** Create a "columns" length real matrix field. */
  def TestMatrixField(columns: Int, matrixShape: Shape) : MatrixField =
    TestMatrixField(Shape(columns), matrixShape, RefMatrixField(columns, matrixShape))

  /** Create a "rows" x "columns" real matrix field. */
  def TestMatrixField(rows: Int, columns: Int, matrixShape: Shape) : MatrixField  =
    TestMatrixField(Shape(rows, columns), matrixShape,
                    RefMatrixField(rows, columns, matrixShape))
  
  /** Create a "layers" x rows" x "columns" real matrix field. */
  def TestMatrixField(layers: Int, rows: Int, columns: Int, matrixShape: Shape) : MatrixField  =
    TestMatrixField(Shape(layers, rows, columns), matrixShape,
                    RefMatrixField(layers, rows, columns, matrixShape))

  def TestMatrixField(initialState: => RefMatrixField) : MatrixField  = {
    // Call initial state function once, rather than allow it to be called once for each read()
    // method invocation in the last routine below.
    val initState: RefMatrixField = initialState
    TestMatrixField(initState.fieldShape, initState.tensorShape, initState)
  }

  def TestMatrixField(fieldShape: Shape, tensorShape: Shape,
                      initialState: => RefMatrixField) : MatrixField = {
    fieldShape.dimensions match {
      case 0 => MatrixField(initialState.read())
      case 1 => MatrixField(fieldShape(0), (x) => initialState.read(x))
      case 2 => MatrixField(fieldShape(0), fieldShape(1), (x, y) => initialState.read(x, y))
      case 3 => MatrixField(fieldShape(0), fieldShape(1), fieldShape(2), 
                            (x, y, z) => initialState.read(x, y, z))
      case x => throw new RuntimeException("illegal number of dimensions"); null
    }

  }
}
