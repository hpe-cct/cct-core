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

import cogx.compiler.parser.syntaxtree.ComplexField
import cogx.cogmath.geometry.Shape
import cogx.reference._

/**
 * Glue code that adds cog3 style initialization of new ComplexFields using
 * RefComplexFields.
 *
 */
trait ComplexFieldBuilderInterface {

  def TestComplexField(initialState: => RefComplexField) : ComplexField = {
    // Call initial state function once, rather than allow it to be called once for each read()
    // method invocation in the last routine below.
    val initState: RefComplexField = initialState
    TestComplexField(initState.fieldShape, initState)
  }

  def TestComplexField(columns: Int) : ComplexField  = 
    TestComplexField(Shape(columns), RefComplexField(columns))

  def TestComplexField(rows: Int, columns: Int)  : ComplexField =
    TestComplexField(Shape(rows, columns), RefComplexField(rows, columns))

  def TestComplexField(layers: Int, rows: Int, columns: Int) : ComplexField =
    TestComplexField(Shape(layers, rows, columns),
                     RefComplexField(layers, rows, columns))

  def TestComplexField(shape: Shape, initialState: => RefComplexField) : ComplexField = {
    
    shape.dimensions match {
      case 0 => ComplexField(initialState.read())
      case 1 => ComplexField(shape(0), (x) => initialState.read(x))
      case 2 => ComplexField(shape(0), shape(1), (x, y) => initialState.read(x, y))
      case 3 => ComplexField(shape(0), shape(1), shape(2), (x, y, z) => initialState.read(x, y, z))
      case x => throw new RuntimeException("illegal number of dimensions"); null
    }
  }
}
