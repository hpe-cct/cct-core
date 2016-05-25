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

package cogx.reference.memory

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import cogx.cogmath.geometry.Shape
import cogx.cogmath.algebra.real.{Scalar, Matrix}
import java.util.Random
import cogx.platform.types.FieldType
import cogx.platform.types.ElementTypes.Float32
import scala.collection.mutable.ArrayBuffer

/** Test code.
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class RefFieldMemorySpec extends FunSuite with MustMatchers {
  val random = new Random
  val fieldShape = Shape(5, 7, 11)
  val fieldPoints = fieldShape.points
  val scalarShape = Shape()
  val matrixShape = Shape(2, 3)
  val scalarFieldType = new FieldType(fieldShape, scalarShape, Float32)
  val matrixFieldType = new FieldType(fieldShape, matrixShape, Float32)
  val scalarField = new RefFieldMemory(scalarFieldType)
  val matrixField = new RefFieldMemory(matrixFieldType)

  test("Constructors") {
    val random = new Random
    def randomScalar = random.nextFloat
    def randomMatrix = new Matrix(matrixShape(0), matrixShape(1)).randomize
    val scalars = new Array[Float](fieldPoints)
    val matrices = new Array[Matrix](fieldPoints)
    for (i <- 0 until fieldPoints) {
      scalars(i) = randomScalar
      matrices(i) = randomMatrix
    }
    val scalarBuffer = RefFieldMemory.fromFloats(scalarFieldType, scalars)
    require((scalarBuffer zip scalars).
            map(v => v._1.read(0) == v._2).reduceLeft(_ && _))
    val matrixBuffer = RefFieldMemory(matrixFieldType, matrices)
    require((matrixBuffer zip matrices).
            map(v => new Matrix(v._1) == v._2).reduceLeft(_ && _))
  }

  test("Scalar") {
    //val fType = new FieldType(fieldShape, Shape(), RealElement)
    //val layout = new FieldMemoryLayout {val fieldType = fType}
    for (index <- fieldShape.indices) {
      val writeData = index.toArray.reduceLeft(_ + _)
      scalarField.write(new Scalar(writeData), index : _*)
      val readData = scalarField.read(index : _*).read(0)
      require(readData == writeData)
      require(scalarField.read(0, 0, 0).read(0) == 0)
    }
    for (index <- fieldShape.indices)
      require(scalarField.read(index : _*).read(0) == index.toArray.reduceLeft(_ + _))
  }

  test("Matrix") {
    val matrices = ArrayBuffer[Matrix]()
    for (index <- fieldShape.indices) {
      val matrix = new Matrix(2, 3).randomize
      matrices += matrix
      matrixField.write(matrix, index : _*)
    }
    var globalIndex = 0
    for (index <- fieldShape.indices) {
      val matrix = matrixField.read(index : _*).asInstanceOf[Matrix]
      require(matrix == matrices(globalIndex))
      globalIndex += 1
    }
  }

  test("Equality") {
    require(scalarField == scalarField)
    require(scalarField ~== scalarField)
  }
}