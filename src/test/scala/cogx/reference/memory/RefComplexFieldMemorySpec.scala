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
import java.util.Random
import cogx.platform.types.FieldType
import cogx.platform.types.ElementTypes.Complex32
import cogx.cogmath.geometry.Shape
import cogx.cogmath.algebra.complex.{Complex, ComplexImplicits}

/** Test code.
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class RefRefComplexFieldMemorySpec extends FunSuite with MustMatchers with ComplexImplicits {
  val random = new Random

  val fieldShape = Shape(5, 7, 11)
  val fieldPoints = fieldShape.points
  val scalarShape = Shape()
  val scalarFieldType = new FieldType(fieldShape, scalarShape, Complex32)
  val scalarField = new RefComplexFieldMemory(scalarFieldType)

  test("Constructors") {
    val random = new Random
    def randomScalar = Complex(random.nextFloat, random.nextFloat)
    val scalars = new Array[Complex](fieldPoints)
    for (i <- 0 until fieldPoints) {
      scalars(i) = randomScalar
    }
    val scalarField = RefComplexFieldMemory(scalarFieldType, scalars)
    require((scalarField zip scalars).map(v => v._1 == v._2).reduceLeft(_ && _))
  }

  test("Scalar") {
    def makeScalar(index: Array[Int]) = Complex(index(0), index(1))
    for (index <- fieldShape.indices)
      scalarField.write(makeScalar(index), index : _*)
    for (index <- fieldShape.indices)
      require(scalarField.read(index : _*) == makeScalar(index))
  }

  test("Equality") {
    require(scalarField == scalarField)
    require(scalarField ~== scalarField)
  }
}