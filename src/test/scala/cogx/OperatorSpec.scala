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
import cogx.api.ImplicitConversions
import cogx.helper.{ColorFieldBuilderInterface, ScalarFieldBuilderInterface}
import cogx.reference.RefTestInterface

//import cogx.cogmath.algebra.real.Vector


/** Test code for user-defined operators
  */

@RunWith(classOf[JUnitRunner])
class OperatorSpec
        extends FunSuite
        with MustMatchers
        with ImplicitConversions
        with ScalarFieldBuilderInterface
        with ColorFieldBuilderInterface
{
  val Optimize = true

  test("scalar field operator") {
    // Flip an image upside down.
    object flip extends Operator {
      def compute(in: ScalarFieldReader, out: ScalarFieldWriter) {
        out.setShape(in.fieldShape)
        for (row <- 0 until in.rows; col <- 0 until in.columns) {
          val pixel = in.read(row, col)
          out.write(in.rows - row - 1, col, pixel)
        }
      }
    }
    val graph = new ComputeGraph(false) with RefTestInterface {
      val in = ScalarField.random(5, 5)
      val flipped = flip(in)
      val flippedTwice = flip(flipped)
      val flippedThrice = flip(flippedTwice)
    }
    import graph._
    withRelease {
      step
      require(readScalar(in) == readScalar(flippedTwice))
      require(!(readScalar(in) == readScalar(flipped)))
      require(readScalar(flipped) == readScalar(flippedThrice))
    }
  }

  test("operator chain") {
    // Flip an image upside down.
    object flip extends Operator {
      def compute(in: ScalarFieldReader, out: ScalarFieldWriter) {
        out.setShape(in.fieldShape)
        for (row <- 0 until in.rows; col <- 0 until in.columns) {
          val pixel = in.read(row, col)
          out.write(in.rows - row - 1, col, pixel)
        }
      }
    }
    val graph = new ComputeGraph(false) with RefTestInterface {
      val in = ScalarField.random(5, 5)
      val flipped = flip(in)
      val flippedTwice = flip(flip(in))
      val flippedThrice = flip(flip(flip(in)))
    }
    import graph._
    withRelease {
      step
      require(readScalar(in) == readScalar(flippedTwice))
      require(!(readScalar(in) == readScalar(flipped)))
      require(readScalar(flipped) == readScalar(flippedThrice))
    }
  }

  test("vector field operator") {
    // Flip an image upside down.
    object flip extends Operator {
      def compute(in: VectorFieldReader, out: VectorFieldWriter) {
        out.setShape(in.fieldShape, in.tensorShape)
        val vector = new Vector(in.tensorShape(0))
        for (row <- 0 until in.rows; col <- 0 until in.columns) {
          in.read(row, col, vector)
          out.write(in.rows - row - 1, col, vector)
        }
      }
    }
    val graph = new ComputeGraph(false) with RefTestInterface {
      val in = VectorField.random(Shape(5, 5), Shape(3))
      val flipped = flip(in)
      val flippedTwice = flip(flipped)
      val flippedThrice = flip(flippedTwice)
    }
    import graph._
    withRelease {
      step
      require(readVector(in) == readVector(flippedTwice))
      require(!(readVector(in) == readVector(flipped)))
      require(readVector(flipped) == readVector(flippedThrice))
    }
  }

  test("matrix field operator") {
    // Flip an image upside down.
    object flip extends Operator {
      def compute(in: MatrixFieldReader, out: MatrixFieldWriter) {
        out.setShape(in.fieldShape, in.tensorShape)
        val matrix = new Matrix(in.tensorShape(0), in.tensorShape(1))
        for (row <- 0 until in.rows; col <- 0 until in.columns) {
          in.read(row, col, matrix)
          out.write(in.rows - row - 1, col, matrix)
        }
      }
    }
    val graph = new ComputeGraph(false) with RefTestInterface {
      val in = MatrixField.random(Shape(3, 4), Shape(5, 7))
      val flipped = flip(in)
      val flippedTwice = flip(flipped)
      val flippedThrice = flip(flippedTwice)
    }
    import graph._
    withRelease {
      step
      require(readMatrix(in) == readMatrix(flippedTwice))
      require(!(readMatrix(in) == readMatrix(flipped)))
      require(readMatrix(flipped) == readMatrix(flippedThrice))
    }
  }

  test("color field operator") {
    // Flip an image upside down.
    object flip extends Operator {
      def compute(in: ColorFieldReader, out: ColorFieldWriter) {
        out.setShape(in.fieldShape)
        val pixel = new Pixel
        for (row <- 0 until in.rows; col <- 0 until in.columns) {
          in.read(row, col, pixel)
          out.write(in.rows - row - 1, col, pixel)
        }
      }
    }
    val graph = new ComputeGraph(false) with RefTestInterface {
      val in = ColorField.random(5, 5)
      val flipped = flip(in)
      val flippedTwice = flip(flipped)
      val flippedThrice = flip(flippedTwice)
    }
    import graph._
    withRelease {
      step
      require(readColor(in) == readColor(flippedTwice))
      require(!(readColor(in) == readColor(flipped)))
      require(readColor(flipped) == readColor(flippedThrice))
    }
  }

  test("complex field operator") {
    // Flip an image upside down.
    object flip extends Operator {
      def compute(in: ComplexFieldReader, out: ComplexFieldWriter) {
        out.setShape(in.fieldShape)
        for (row <- 0 until in.rows; col <- 0 until in.columns) {
          val complex = in.read(row, col)
          out.write(in.rows - row - 1, col, complex)
        }
      }
    }
    val graph = new ComputeGraph(false) with RefTestInterface {
      val in = ComplexField.random(5, 5)
      val flipped = flip(in)
      val flippedTwice = flip(flipped)
      val flippedThrice = flip(flippedTwice)
    }
    import graph._
    withRelease {
      step
      require(readComplex(in) == readComplex(flippedTwice))
      require(!(readComplex(in) == readComplex(flipped)))
      require(readComplex(flipped) == readComplex(flippedThrice))
    }
  }

  test("binary operator") {
    // Add two images
    object add2 extends Operator {
      def compute(in1: ScalarFieldReader, in2: ScalarFieldReader, out: ScalarFieldWriter) {
        out.setShape(in1.fieldShape)
        for (row <- 0 until in1.rows; col <- 0 until in1.columns) {
          val pixel1 = in1.read(row, col)
          val pixel2 = in2.read(row, col)
          out.write(row, col, pixel1 + pixel2)
        }
      }
    }
    val graph = new ComputeGraph(false) with RefTestInterface {
      val in1 = ScalarField.random(5, 5)
      val in2 = ScalarField.random(5, 5)
      val summed = add2(in1, in2)
    }
    import graph._
    withRelease {
      step
      require((readScalar(in1) + readScalar(in2)) == readScalar(summed))
    }
  }

  test("mixed operator") {
    // Add vector field and scalar field
    object add2 extends Operator {
      def compute(in1: VectorFieldReader, in2: ScalarFieldReader, out: VectorFieldWriter) {
        out.setShape(in1.fieldShape, in1.tensorShape)
        val vector = new Vector(in1.tensorShape(0))
        for (row <- 0 until in1.rows; col <- 0 until in1.columns) {
          in1.read(row, col, vector)
          val pixel2 = in2.read(row, col)
          out.write(row, col, vector + pixel2)
        }
      }
    }
    val graph = new ComputeGraph(false) with RefTestInterface {
      val in1 = VectorField.random(Shape(5, 6), Shape(7))
      val in2 = ScalarField.random(5, 6)
      val summed = add2(in1, in2)
    }
    import graph._
    withRelease {
      step
      require((readVector(in1) + readScalar(in2)) == readVector(summed))
    }
  }
}