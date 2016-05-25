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

import cogx.compiler.parser.syntaxtree.VectorField
import cogx.cogmath.geometry.Shape
import cogx.reference._
import cogx.cogmath.algebra.real.Vector

/**
 * Glue code that adds cog3 style initialization of new VectorFields using
 * RefVectorFields.
 *
 */
trait VectorFieldBuilderInterface {


  /** Create a 0D real scalar field. */
  def TestVectorField(vectorShape: Shape): VectorField  =
    TestVectorField(Shape(), vectorShape, RefVectorField(vectorShape))

  /** Create a "columns" length real scalar field. */
  def TestVectorField(columns: Int, vectorShape: Shape) : VectorField =
    TestVectorField(Shape(columns), vectorShape, RefVectorField(columns, vectorShape))

  /** Create a "rows" x "columns" real scalar field. */
  def TestVectorField(rows: Int, columns: Int, vectorShape: Shape) : VectorField  =
    TestVectorField(Shape(rows, columns), vectorShape,
      RefVectorField(rows, columns, vectorShape))

  /** Create a "layers" x "rows" x "columns" real scalar field. */
  def TestVectorField(layers: Int, rows: Int, columns: Int, vectorShape: Shape): VectorField  =
    TestVectorField(Shape(layers, rows, columns), vectorShape,
      RefVectorField(layers, rows, columns, vectorShape))

  /*def TestVectorField(fieldType: FieldType): VectorField  =
    TestVectorField(fieldType.fieldShape, fieldType.tensorShape,
      RefVectorField(fieldType.fieldShape, fieldType.tensorShape))
    */
  def TestVectorField(initialState: => RefVectorField) : VectorField = {
    // Call initial state function once, rather than allow it to be called once for each read()
    // method invocation in the last routine below.
    val initState: RefVectorField = initialState
    TestVectorField(initState.fieldShape, initState.tensorShape, initState)
  }

  def TestVectorField(fieldShape: Shape, tensorShape: Shape,  
                      initialState: => RefVectorField): VectorField = {
    fieldShape.dimensions match {
      case 0 => VectorField(initialState.read())
      case 1 => VectorField(fieldShape(0), (x) => initialState.read(x))
      case 2 => VectorField(fieldShape(0), fieldShape(1), (x, y) => initialState.read(x, y))
      case 3 => VectorField(fieldShape(0), fieldShape(1), fieldShape(2), 
                            (x, y, z) => initialState.read(x, y, z))
      case x => throw new RuntimeException("illegal number of dimensions"); null
    }
  }

}
