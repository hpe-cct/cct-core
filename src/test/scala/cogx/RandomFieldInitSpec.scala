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

package cogx

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import org.junit.runner.RunWith
import cogx.test._
import cogx.reference.RefTestInterface
import cogx.api.ImplicitConversions
import cogx.helper.{VectorFieldBuilderInterface, MatrixFieldBuilderInterface, ComplexFieldBuilderInterface, ScalarFieldBuilderInterface}

/** Test code for the random initialization of fields
  *
  * @author Dick Carter
  */

@RunWith(classOf[JUnitRunner])
class RandomFieldInitSpec extends FunSuite
                          with MustMatchers
                          with ImplicitConversions
                          with ScalarFieldBuilderInterface
                          with ComplexFieldBuilderInterface
                          with MatrixFieldBuilderInterface
                          with VectorFieldBuilderInterface
                          with CogMatchers {

  test("ScalarFields same after each reset") {

    val graph = new ComputeGraph(optimize = true) with RefTestInterface {
      val scalar0D = ScalarField.random()
      val scalar1D = ScalarField.random(5)
      val scalar2D = ScalarField.random(3, 5)
      val scalar3D = ScalarField.random(4, 5, 7)

      probe(scalar0D, scalar1D, scalar2D, scalar3D)
    }

    import graph._
    withRelease {
      reset
      val scalar0D_1 = readScalar(scalar0D)
      val scalar1D_1 = readScalar(scalar1D)
      val scalar2D_1 = readScalar(scalar2D)
      val scalar3D_1 = readScalar(scalar3D)
      reset
      require(readScalar(scalar0D) == scalar0D_1)
      require(readScalar(scalar1D) == scalar1D_1)
      require(readScalar(scalar2D) == scalar2D_1)
      require(readScalar(scalar3D) == scalar3D_1)
    }
  }

  test("ScalarField filters same to multiple fast convolutions") {

    val graph = new ComputeGraph(optimize = true, fftUse = UseFFTAlways) with RefTestInterface {
      val one = ScalarField(3,3, (_,_) => 1f)
      val oneToo = ScalarField(3,3, (_,_) => 1f)

      // keep any constant-folding optimizer from combining the above
      one <== oneToo
      oneToo <== one

      val filter = ScalarField.random(3, 3)

      val oneConvolved = convolve(one, filter, BorderClamp)
      val oneTooConvolved = convolve(oneToo, filter, BorderClamp)

      probe(oneConvolved, oneTooConvolved)
    }

    import graph._
    withRelease {
      reset
      require(readScalar(oneConvolved) == readScalar(oneTooConvolved))
    }
  }

  test("VectorFields same after each reset") {

    val graph = new ComputeGraph(optimize = true) with RefTestInterface {
      val vector0D = VectorField.random(Shape(), Shape(3))
      val vector1D = VectorField.random(Shape(5), Shape(4))
      val vector2D = VectorField.random(Shape(3, 5), Shape(11))
      val vector3D = VectorField.random(Shape(4, 5, 7), Shape(2))

      probe(vector0D, vector1D, vector2D, vector3D)
    }

    import graph._
    withRelease {
      reset
      val vector0D_1 = readVector(vector0D)
      val vector1D_1 = readVector(vector1D)
      val vector2D_1 = readVector(vector2D)
      val vector3D_1 = readVector(vector3D)
      reset
      require(readVector(vector0D) == vector0D_1)
      require(readVector(vector1D) == vector1D_1)
      require(readVector(vector2D) == vector2D_1)
      require(readVector(vector3D) == vector3D_1)
    }
  }

  test("MatrixFields same after each reset") {

    val graph = new ComputeGraph(optimize = true) with RefTestInterface {
      val matrix0D = MatrixField.random(Shape(), Shape(3, 4))
      val matrix1D = MatrixField.random(Shape(5), Shape(4, 2))
      val matrix2D = MatrixField.random(Shape(3, 5), Shape(11, 3))
      val matrix3D = MatrixField.random(Shape(4, 5, 7), Shape(2, 5))

      probe(matrix0D, matrix1D, matrix2D, matrix3D)
    }

    import graph._
    withRelease {
      reset
      val matrix0D_1 = readMatrix(matrix0D)
      val matrix1D_1 = readMatrix(matrix1D)
      val matrix2D_1 = readMatrix(matrix2D)
      val matrix3D_1 = readMatrix(matrix3D)
      reset
      require(readMatrix(matrix0D) == matrix0D_1)
      require(readMatrix(matrix1D) == matrix1D_1)
      require(readMatrix(matrix2D) == matrix2D_1)
      require(readMatrix(matrix3D) == matrix3D_1)
    }
  }

  test("ComplexFields same after each reset") {

    val graph = new ComputeGraph(optimize = true) with RefTestInterface {
      val complex0D = ComplexField.random()
      val complex1D = ComplexField.random(5)
      val complex2D = ComplexField.random(3, 5)
      val complex3D = ComplexField.random(4, 5, 7)

      probe(complex0D, complex1D, complex2D, complex3D)
    }

    import graph._
    withRelease {
      reset
      val complex0D_1 = readComplex(complex0D)
      val complex1D_1 = readComplex(complex1D)
      val complex2D_1 = readComplex(complex2D)
      val complex3D_1 = readComplex(complex3D)
      reset
      require(readComplex(complex0D) == complex0D_1)
      require(readComplex(complex1D) == complex1D_1)
      require(readComplex(complex2D) == complex2D_1)
      require(readComplex(complex3D) == complex3D_1)
    }
  }

  test("ColorFields same after each reset") {

    val graph = new ComputeGraph(optimize = true) with RefTestInterface {
      val color2D = ColorField.random(3, 5)

      probe(color2D)
    }

    import graph._
    withRelease {
      reset
      val color2D_1 = readColor(color2D)
      reset
      require(readColor(color2D) == color2D_1)
    }
  }

}
