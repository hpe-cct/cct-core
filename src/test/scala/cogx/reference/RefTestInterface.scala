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

package cogx.reference

import cogx.compiler.parser.syntaxtree._
import cogx.platform.cpumemory.readerwriter.FieldReader
import cogx.platform.cpumemory._
import cogx.cogmath.algebra.complex.ComplexVector
import cogx.platform.types.Pixel

/** Glue code that converts each *Field class to its Ref*Field equivalent for
  * testing.
  *
  * @author Greg Snider
  */
trait RefTestInterface {

  // Abstract method.  Avaliable from ComputeGraph.
  def read(f: Field): FieldReader

  /** Convert ScalarField `field` to a RefScalarField. */
  def readScalar(field: Field): RefScalarField = {
    val array: Array[Float] = read(field).asInstanceOf[ScalarFieldMemory].readAsPaddedArray
    new RefScalarField(field.fieldType.fieldShape, array)
  }

  /** Convert ComplexField `field` to a RefComplexField. */
  def readComplex(field: Field): RefComplexField = {
    val array: Array[Float] = read(field).asInstanceOf[ComplexFieldMemory].readAsPaddedArray
    new RefComplexField(field.fieldType.fieldShape, array)
  }

  // The Cog 3.x "reference" code did not have complex vector fields, so we supply
  // some "slice" operations that return a portion of a ComplexVectorField in
  // terms of the existing reference types, namely complex scalar fields (a
  // "tensor slice") and vector fields (real and imaginary parts).

  /** Convert one plane of a ComplexVectorField `field` to a RefComplexField. */
  def readComplexPlane(plane: Int)(field: Field): RefComplexField = {
    val fieldShape = field.fieldType.fieldShape
    require(field.fieldType.tensorOrder == 1)

    val complexVectorFieldMem = read(field).asInstanceOf[ComplexVectorFieldMemory]
    val complexVector = new ComplexVector(field.fieldType.tensorShape.points)
    field.fieldType.dimensions match {
      case 3 =>
        RefComplexField(fieldShape(0), fieldShape(1), fieldShape(2),
          (l: Int, r: Int, c: Int) => {
            complexVectorFieldMem.read(l, r, c, complexVector)
            complexVector(plane)
          })
      case 2 =>
        RefComplexField(fieldShape(0), fieldShape(1),
          (r: Int, c: Int) => {
            complexVectorFieldMem.read(r, c, complexVector)
            complexVector(plane)
          })
      case 1 =>
        RefComplexField(fieldShape(0),
          (c: Int) => {
            complexVectorFieldMem.read(c, complexVector)
            complexVector(plane)
          })
      case 0 =>
        complexVectorFieldMem.read(complexVector)
        RefComplexField(complexVector(plane))
    }
  }

  /** Convert real part of a ComplexVectorField `field` to a RefVectorField. */
  def readRealVector(field: Field): RefVectorField = {
    val fieldShape = field.fieldType.fieldShape
    require(field.fieldType.tensorOrder == 1)

    val complexVectorFieldMem = read(field).asInstanceOf[ComplexVectorFieldMemory]
    val complexVector = new ComplexVector(field.fieldType.tensorShape.points)
    field.fieldType.dimensions match {
      case 3 =>
        RefVectorField(fieldShape(0), fieldShape(1), fieldShape(2),
          (l: Int, r: Int, c: Int) => {
            complexVectorFieldMem.read(l, r, c, complexVector)
            complexVector.real
          })
      case 2 =>
        RefVectorField(fieldShape(0), fieldShape(1),
          (r: Int, c: Int) => {
            complexVectorFieldMem.read(r, c, complexVector)
            complexVector.real
          })
      case 1 =>
        RefVectorField(fieldShape(0),
          (c: Int) => {
            complexVectorFieldMem.read(c, complexVector)
            complexVector.real
          })
      case 0 =>
        complexVectorFieldMem.read(complexVector)
        RefVectorField(complexVector.real)
    }
  }

  /** Convert imaginary part of a ComplexVectorField `field` to a RefVectorField. */
  def readImaginaryVector(field: Field): RefVectorField = {
    val fieldShape = field.fieldType.fieldShape
    require(field.fieldType.tensorOrder == 1)

    val complexVectorFieldMem = read(field).asInstanceOf[ComplexVectorFieldMemory]
    val complexVector = new ComplexVector(field.fieldType.tensorShape.points)
    field.fieldType.dimensions match {
      case 3 =>
        RefVectorField(fieldShape(0), fieldShape(1), fieldShape(2),
          (l: Int, r: Int, c: Int) => {
            complexVectorFieldMem.read(l, r, c, complexVector)
            complexVector.imaginary
          })
      case 2 =>
        RefVectorField(fieldShape(0), fieldShape(1),
          (r: Int, c: Int) => {
            complexVectorFieldMem.read(r, c, complexVector)
            complexVector.imaginary
          })
      case 1 =>
        RefVectorField(fieldShape(0),
          (c: Int) => {
            complexVectorFieldMem.read(c, complexVector)
            complexVector.imaginary
          })
      case 0 =>
        complexVectorFieldMem.read(complexVector)
        RefVectorField(complexVector.imaginary)
    }
  }

  /** Convert VectorField `field` to a RefVectorField. */
  def readVector(field: Field): RefVectorField = {
    val array: Array[Float] = read(field).asInstanceOf[VectorFieldMemory].readAsPaddedArray
    new RefVectorField(field.fieldType.fieldShape, field.fieldType.tensorShape, array)
  }

  /** Convert MatrixField `field` to a RefMatrixField. */
  def readMatrix(field: Field): RefMatrixField = {
    val array: Array[Float] = read(field).asInstanceOf[MatrixFieldMemory].readAsPaddedArray
    new RefMatrixField(field.fieldType.fieldShape, field.fieldType.tensorShape, array)
  }

  /** Convert ColorField `field` to a RefColorField. */
  def readColor(field: Field): RefColorField = {
    require(field.fieldType.dimensions == 2)
    val Rows = field.fieldType.fieldShape(0)
    val Columns = field.fieldType.fieldShape(1)
    val colorFieldMem = read(field).asInstanceOf[ColorFieldMemory]
    val pixel = new Pixel

    new RefColorField(Rows, Columns, (r: Int, c: Int) => {
      colorFieldMem.read(r, c, pixel)
      (pixel.redFloat, pixel.greenFloat, pixel.blueFloat)
    })
  }
}
